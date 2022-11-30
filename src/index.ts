import { Elm } from "./elm/Main.elm"



const elm = Elm.Main.init({})

const toElm = elm.ports.portReceive.send

const subscribeHandler = (m: string) => {
  toElm(m)
}

elm.ports.portSend.subscribe(subscribeHandler)

