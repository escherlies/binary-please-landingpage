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
    flags?: {
        // Elm Flags
    }
}


type Ports = {
    portReceive: { send: (msg: string) => void }
    portSend: { subscribe: (callback: (message: string) => void) => void }
}
