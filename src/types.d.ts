declare module "*.elm" {
  export const Elm: {
    Main: {
      init: (options: Options) => {
        ports: Ports
      }
    }
  }
}


type Options = {
  node?: HTMLElement
  flags?: unknown
}


type Ports = {
  portReceive: { send: (msg: unknown) => void }
  portSend: { subscribe: (callback: (message: string) => void) => void }
}
