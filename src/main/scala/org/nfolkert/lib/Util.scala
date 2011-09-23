package org.nfolkert.lib

import org.joda.time.DateTime
import java.util.Date
import net.liftweb.common.Loggable
import net.liftweb.http.S
import net.liftweb.util.Helpers._

object Util extends Loggable {
  // All of these are in seconds
  def nowInSeconds = System.currentTimeMillis / 1000
  def secondsFromDate(date: Date) = date.getTime / 1000
  def secondsFromDate(date: DateTime) = date.getMillis / 1000
  def dateFromSeconds(s: Long) = new DateTime(s * 1000L)

  def duration[T](f: => T): (T, Long) = {
    val start = System.currentTimeMillis
    val ret: T = f
    (ret, System.currentTimeMillis - start)
  }

  def logTime[T](name: String)(f: => T): T = {
    val (ret, time): (T, Long) = duration[T](f)
    logger.info(name + " took " + time + " ms")
    ret
  }

  def printReq: (String, String) = {
    S.request.map(r => {
      val liftReqMsg = r.toString() + "; stateless: " + r.stateless_?
      val h = r.request
      val httpReqMsg = "Http Request: " + List("cookies" -> h.cookies,
           "provider" -> h.provider,
           "authType" -> h.authType,
           "headers" -> h.headers,
           "contextPath" -> h.contextPath,
           "context" -> h.context,
           "contentType" -> h.contentType,
           "uri" -> h.uri,
           "url" -> h.url,
           "queryString" -> h.queryString,
           "params" -> h.params,
           "session" -> h.session,
           "sessionId" -> h.sessionId,
           "remoteAddress" -> h.remoteAddress,
           "remotePort" -> h.remotePort,
           "remoteHost" -> h.remoteHost,
           "serverName" -> h.serverName,
           "scheme" -> h.scheme,
           "serverPort" -> h.serverPort,
           "method" -> h.method,
           "suspendResumeSupport_?" -> h.suspendResumeSupport_?,
           "resumeInfo" -> h.resumeInfo,
           "multipartContent_?" -> h.multipartContent_?,
           "locale" -> h.locale,
           "userAgent" -> h.userAgent
      ).map(p=>"\t" + p.toString).join("\n")

      (liftReqMsg, httpReqMsg)
    }).openOr(("No Request", "No Request"))
  }
}

object T{def apply[T](name: String)(f: => T): T = Util.logTime(name)(f)}
