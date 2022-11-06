# Elm-Embed-Boilerplate

Simple boilerplate to develop widgets or components with Elm using web components.

## About

For developing standalone widgets or components in Elm. They will have their own state and runtime so you can use them multiple times.

## Config

Whats included?

### [Elm](https://elm-lang.org/)  

Because writing declarative, purely functional web apps is fun!

### [Elm-UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)

Because no one wants to write CSS!

### [Parcel](https://parceljs.org/)

Because it just works right out-of-the-box.

## Example usage

Import the script and use the custom component.

```html
<!DOCTYPE html>

<head>
  <!-- ... -->
  <script src="widget-path/index.js"></script>
  <!-- ... -->
</head>

<body>
  <!-- ... -->
  <elm-counter />
  <elm-counter />
  <!-- ... -->
</body>
```

## Todo

- [ ] Add component interop via flags and ports (advanced use)
