// Domain - "nouns"

type WidgetCode = WidgetCode of string // A wrapper type. Starting with "W" then 4 digits
type GizmoCode = GizmoCode of string // A wrapper type. Starting with "G" then 3 digits

type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

type UnitQuantity = UnitQuantity of int // A wrapper type. Between 1 and 100
type KilogramQuantity = KilogramQuantity of decimal // A wrapper type. Between 0.05 and 100.0

type OrderQuantity =
    | Units of UnitQuantity
    | Kilograms of KilogramQuantity

type Undefined = exn

type CustomerInfo = Undefined
type ShippingAddress = Undefined
type BillingAddress = Undefined
type OrderLine = Undefined
type AmountToBill = Undefined

type Order =
    { CustomerInfo: CustomerInfo
      ShippingAddress: ShippingAddress
      BillingAddress: BillingAddress
      OrderLines: OrderLine list
      AmountToBill: AmountToBill }

type UnvalidatedOrder = Undefined
type ValidatedOrder = Undefined

// Domain - "verbs"

type ValidationError =
    { FieldName: string
      ErrorDescription: string }

// type ValidationResponse<'a> = Async<Result<'a, ValidationError list>>

type ValidateOrder = UnvalidatedOrder -> Async<Result<ValidatedOrder, ValidationError list>>


// examples

let widgetCode = WidgetCode "W1234"
let gizmoCode = GizmoCode "G123"

let processWidgetCode (WidgetCode widgetCode) =
    printfn "Processing widget code %s" widgetCode

processWidgetCode widgetCode
