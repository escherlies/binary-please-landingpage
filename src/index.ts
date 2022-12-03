import { Elm } from "./elm/Main.elm"



const prefersLightColorSheme = window.matchMedia("(prefers-color-scheme: light)")

const elm = Elm.Main.init({
  flags: {
    prefersColorScheme: prefersLightColorSheme.matches ? "light" : "dark",
    window: window,
  }
})

const toElm = elm.ports.portReceive.send

const subscribeHandler = (m: string) => {
  toElm(m)
}

elm.ports.portSend.subscribe(subscribeHandler)



// handle theme change
prefersLightColorSheme.addEventListener("change", mq => {
  toElm({ tag: "UpdatePrefersColorScheme", value: mq.matches ? "light" : "dark" })
})

window.addEventListener("resize", () => {
  toElm({ tag: "UpdateBrowserWindow", value: window })
})
