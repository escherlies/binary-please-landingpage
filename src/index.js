import { Elm } from "./Main.elm";

class ElmCounter extends HTMLElement {
    constructor() {
        super()
        Elm.Main.init({ node: this })
    }
}

customElements.define("elm-counter", ElmCounter);
