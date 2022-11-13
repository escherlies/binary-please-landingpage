import { Elm } from "./Main.elm"

class ElmCounter extends HTMLElement {
  toElm: (msg: string) => void = () => null

  constructor() { super() }

  connectedCallback() {
    const root = document.createElement("div")
    this.appendChild(root)

    const elm = Elm.Main.init({ node: root })
    elm.ports.portSend.subscribe(this.subscribeHandler)
    this.toElm = elm.ports.portReceive.send
  }

  attributeChangedCallback() {
    this.toElm("Attribute changed")
  }

  static get observedAttributes() {
    // List of observed attributes
    return ["testattribute"]
  }


  subscribeHandler = (m: string) => {
    this.toElm(m)
  }

}

customElements.define("elm-counter", ElmCounter)
