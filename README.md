purescript-emmet
==

A library for [emmet-like](https://docs.emmet.io/abbreviations/) html
abbreviations and generators in PureScript.

## Abbreviations

The following subset of the Emmet spec is currently supported:

### Elements

Simple elements:

`div` → `<div></div>`

Classes:

`div.classname` → `<div class="classname"></div>`

Ids:
`div#myId` → `<div id="myId"></div>`

### Nesting

Child:
`div>ul` → `<div><ul></ul></div>`

Sibling:
`div+ul` → `<div></div><ul></ul>`

Grouping:
`div>ul+div` →

```html 
<div>
    <ul>
    <ul/>
<div>
<div/>
<div/
```

`(div>ul)+div` →

```html
<div>
    <ul>
    <ul/>
    <div>
    <div/>
<div/>
```

### Modifiers

Multiplication:
`div*2` → `<div></div><div></div>`

### Example

These different pieces can be composed to build up fairly complex HTML
structures.

## Renderers

An HTML as well as a Halogen renderer are provided.



