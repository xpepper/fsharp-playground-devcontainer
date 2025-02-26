#load "order-taking.fsx"
open OrderTaking.Domain

let widgetCode = WidgetCode "W1234"
let gizmoCode = GizmoCode "G123"

let processWidgetCode (WidgetCode widgetCode) =
    printfn "Processing widget code %s" widgetCode

processWidgetCode widgetCode

let unitQuantity = 
 UnitQuantity.create 10

match unitQuantity with
| Ok(quantity) -> 
    printfn "Success: value is %A" quantity
    let innerValue = UnitQuantity.value quantity
    printfn "Inner value is %i" innerValue
| Error(message) -> printfn "Failure: %s" message

