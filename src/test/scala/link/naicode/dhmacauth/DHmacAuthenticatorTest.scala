package link.naicode.dhmacauth

import link.naicode.dhmacauth.simple.{DirectMappedAuthContextCreator, HashMapAuthKeyStore}
import org.specs2.mutable.Specification
import spray.http.HttpHeaders.{Authorization, `WWW-Authenticate`}
import spray.http.{GenericHttpCredentials, HttpChallenge, HttpResponse}
import spray.routing.AuthenticationFailedRejection.{CredentialsMissing, CredentialsRejected}
import spray.routing.{AuthenticationFailedRejection, Directives, Route}
import spray.testkit.Specs2RouteTest

/**
 * Created by naicode on 9/11/14.
 *
 */

class TestingHashMapAuthKeyStore extends HashMapAuthKeyStore(Map(1.toByte -> 1000,2.toByte -> 2000),3000) {
  def setTimeoutKey(key:Key) = {
    intern.put(key.ident, key.withTimeout(-12345))
  }
}

class DHmacAuthenticatorTest extends Specification with Directives with Specs2RouteTest {
  implicit val contextCreator = DirectMappedAuthContextCreator
  val Ok = HttpResponse()
  val completeOk = complete(Ok)
  val challange0 = `WWW-Authenticate`(HttpChallenge(scheme="dHMACSignature", realm="defaultSecure", params=Map("level" -> "0")))
  val challange2 = `WWW-Authenticate`(HttpChallenge(scheme="dHMACSignature", realm="defaultSecure", params=Map("level" -> "2")))
  val keyIdent = 132

  def finComplete[T]: T => Route = { in => complete(in.toString)}
  def finComplete2[T,R]: (T, R) => Route = { (in1,in2) => complete(s"$in1 + $in2")}
  def queryBase = new TestingHashMapAuthKeyStore()

  def getAuth(realm:String="defaultSecure") = {
    implicit val qb = queryBase
    (qb, DHMACAuth(realm))
  }
  def getNoAuth = {
    getAuth()._2
  }

  def authFor(methode: String, uri: String, key: Key) = {
    val token = Hmac("HmacSHA256").run(s"$methode:$uri:dateval:", key.key).asString
    Authorization(GenericHttpCredentials("dHMACSignature", s"${key.ident}:$token"))
  }

  def withKeyAuth(method:String, uri:String, lvl:Byte = 0, withTimeout:Boolean=false) = {
    var (db, auth) = getAuth()
    auth = auth.withSecurityLevel(lvl)
    val keyStr = "defaultKey"
    val key = Key(keyIdent, keyStr, 0, userId=12, securityLevel=1)
    if (withTimeout) {
      db.setTimeoutKey(key)
    } else {
      db.setKey(key)
    }
    val authTag = authFor(method, uri, key)
    (authTag, auth)
  }

  "the DHmacAuth" should {
    "reject requests without Authorization header with CredentialMissing" in {
      Get() ~> {
        authenticate(getNoAuth) { finComplete }
      } ~> check { rejection === AuthenticationFailedRejection(CredentialsMissing, List(challange0)) }
    }

    "reject request with missing signature" in {
      Get() ~> Authorization(GenericHttpCredentials("dHMACSignature","")) ~> {
        authenticate(getNoAuth) { finComplete }
      } ~> check { rejection === AuthenticationFailedRejection(CredentialsRejected, List(challange0))}
    }

    "reject request with unknowen signature" in {
      Get() ~> Authorization(GenericHttpCredentials("dHMACSignature","abcdefg")) ~> {
        authenticate(getNoAuth) { finComplete }
      } ~> check { rejection === AuthenticationFailedRejection(CredentialsRejected, List(challange0))}
    }

    "extract the userId and key request level from a successfull auth" in {
      val uri = "http://example.com/test"
      val (authTag, auth) = withKeyAuth("GET",uri)
      Get(uri) ~> authTag ~> {
        authenticate(auth) { finComplete }
      } ~> check {responseAs[String] === "(12,1)" }
    }

    "extract the userId and key request level from a successfull auth with same level" in {
      val uri ="http://example.com/test"
      val (authTag, auth) = withKeyAuth("GET",uri,1)
      Get(uri) ~> authTag ~> {
        authenticate(auth) { finComplete }
      } ~> check {responseAs[String] === "(12,1)" }
    }

    "extract the userId and key request level from a successfull auth with POST" in {
      val uri = "http://example.com/test"
      val (authTag, auth) = withKeyAuth("POST",uri)
      Post(uri) ~> authTag ~> {
        authenticate(auth) { finComplete }
      } ~> check {responseAs[String] === "(12,1)" }
    }

    "fail if the key level is to smal" in {
      val uri ="http://example.com/test"
      val (authTag, auth) = withKeyAuth("GET",uri,2)
      Get(uri) ~> authTag ~> {
        authenticate(auth) { finComplete }
      } ~> check {rejection === AuthenticationFailedRejection(CredentialsRejected, List(challange2)) }
    }

    "fail if the key timed out" in {
      val uri ="http://example.com/test"
      val (authTag, auth) = withKeyAuth("GET",uri,0, withTimeout = true)
      Get(uri) ~> authTag ~> {
        authenticate(auth) { finComplete }
      } ~> check {rejection === AuthenticationFailedRejection(CredentialsRejected, List(challange0)) }
    }


    "automaticly refresch key timeouts if refreshKeyOnValide is set" in {
      val uri ="http://example.com/test"
      val (authTag, auth) = withKeyAuth("GET",uri,1)
      val oldKey = auth.authKeyStore.queryForIdent(keyIdent).get
      Get(uri) ~> authTag ~> {
        authenticate(auth) { finComplete }
      } ~> check {
        (responseAs[String] === "(12,1)") and
          (oldKey.timeout must be_<=(auth.authKeyStore.queryForIdent(keyIdent).get.timeout))
      }
    }

    "not refresh timeouts if refreshKeyOnValide is not set" in {
      val uri ="http://example.com/test"
      var (authTag, auth) = withKeyAuth("GET",uri,1)
      auth = auth.withRefreshTimeoutOnValideKeyUsage(doit_? = false)
      val oldKey = auth.authKeyStore.queryForIdent(keyIdent).get
      Get(uri) ~> authTag ~> {
        authenticate(auth) { finComplete }
      } ~> check {
        (responseAs[String] === "(12,1)") and
          (oldKey.timeout must_== auth.authKeyStore.queryForIdent(keyIdent).get.timeout)
      }
    }

    "still not allow timouted keys if refreshKeyOnValide is false" in {
      val uri ="http://example.com/test"
      var (authTag, auth) = withKeyAuth("GET",uri,0, withTimeout = true)
      auth = auth.withRefreshTimeoutOnValideKeyUsage(doit_? = false)
      Get(uri) ~> authTag ~> {
        authenticate(auth) { finComplete }
      } ~> check {rejection === AuthenticationFailedRejection(CredentialsRejected, List(challange0)) }
    }

  }
}
