#load "order-taking.fsx"
open OrderTaking.Domain

let widgetCode = WidgetCode "W1234"
let gizmoCode = GizmoCode "G123"

let processWidgetCode (WidgetCode widgetCode) =
    printfn "Processing widget code %s" widgetCode

processWidgetCode widgetCode
