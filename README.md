# Elm-Embed-Boilerplate

Simple boilerplate to develop widgets or components with Elm using web components.

## About

For developing standalone widgets or components in Elm. They will have their own state and runtime so you can use them multiple times.

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
