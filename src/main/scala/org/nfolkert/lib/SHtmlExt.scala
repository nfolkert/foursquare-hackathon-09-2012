package org.nfolkert.lib

import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import xml.Elem

object SHtmlExt {

  def ajaxRange(min: Double, max: Double, step: Double, value: Double, fn: Double => JsCmd, attrs: SHtml.ElemAttr*): Elem = {
    // There is no lift ajax range slider; only a regular range slider.  Wah.
    import net.liftweb.util.Helpers._
    import net.liftweb.http.js.JE.JsRaw

    val fHolder = S.LFuncHolder(in => in.headOption.flatMap(asDouble(_)).map(fn(_)).getOrElse(JsCmds.Noop))

    val raw = (funcName: String, value: String) => JsRaw("'" + funcName + "=' + encodeURIComponent(" + value + ".value)")
    val key = S.formFuncName

    S.fmapFunc(S.contextFuncBuilder(fHolder)) {
      funcName =>
        <input type="range" min={min.toString} max={max.toString} step={step.toString} value={value.toString} onchange={SHtml.makeAjaxCall(raw(funcName, "this")).toJsCmd}/>
    }
  }
}