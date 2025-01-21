namespace OrderTaking.Domain

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

type OrderId = Undefined
type OrderLineId = Undefined
type CustomerId = Undefined

type CustomerInfo = Undefined
type ShippingAddress = Undefined
type BillingAddress = Undefined
type Price = Undefined
type BillingAmount = Undefined

type Order = { 
    Id: OrderId
    CustomerId: CustomerId
    CustomerInfo: CustomerInfo
    ShippingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: OrderLine list
    AmountToBill: BillingAmount } 

and OrderLine = { 
    Id: OrderLineId
    OrderId: OrderId
    ProductCode: ProductCode
    Quantity: OrderQuantity
    Price: Price 
}

type UnvalidatedOrder = {
    OrderId: string
    CustomerInfo: string
    ShippingAddress: string
}

type PlaceOrderEvents = {
    AcknowledgementSent: Undefined
    OrderPlaced: Undefined
    BillableOrderPlaced: Undefined
}

type PlaceOrderError = 
    | ValidationError of ValidationError list
    | ProductNotFoundError of ProductCode

and ValidationError =
    { FieldName: string
      ErrorDescription: string }

// Domain - "verbs"

type PlaceOrder = UnvalidatedOrder -> Result<PlaceOrderEvents, PlaceOrderError>
