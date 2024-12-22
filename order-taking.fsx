type WidgetCode = WidgetCode of string // A wrapper type. Starting with "W" then 4 digits
type GizmoCode = GizmoCode of string // A wrapper type. Starting with "G" then 3 digits

type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

[<Struct>]
type UnitQuantity = UnitQuantity of int // A wrapper type. Between 1 and 100
[<Struct>]
type KilogramQuantity = KilogramQuantity of decimal // A wrapper type. Between 0.05 and 100.0

type OrderQuantity =
    | Units of UnitQuantity
    | Kilograms of KilogramQuantity

let widgetCode = WidgetCode "W1234"
let gizmoCode = GizmoCode "G123"

let processWidgetCode (WidgetCode widgetCode) = printfn "Processing widget code %s" widgetCode

processWidgetCode widgetCode
