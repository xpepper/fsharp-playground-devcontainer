namespace OrderTaking.Domain

[<Measure>]
type kg

// Domain - "nouns"

type WidgetCode = WidgetCode of string // A wrapper type. Starting with "W" then 4 digits
type GizmoCode = GizmoCode of string // A wrapper type. Starting with "G" then 3 digits

type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

type UnitQuantity = private UnitQuantity of int

module UnitQuantity =
    // Define a "smart constructor" for UnitQuantity​​
    // int -> Result<UnitQuantity,string>​
    let create quantity =
        if quantity < 1 then
            Error "Quantity must be at least 1"
        elif quantity > 1000 then
            Error "Quantity must be at most 1000"
        else
            Ok(UnitQuantity quantity)

    let value (UnitQuantity quantity) = quantity

type KilogramQuantity = KilogramQuantity of decimal<kg> // A wrapper type. Between 0.05 and 100.0

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

// type NonEmptyList<'a> = {
//     First: 'a
//     Rest: 'a list
// }

type Order =
    { Id: OrderId
      CustomerId: CustomerId
      CustomerInfo: CustomerInfo
      ShippingAddress: ShippingAddress
      BillingAddress: BillingAddress
      // OrderLines: NonEmptyList<OrderLine>
      OrderLines: OrderLine list
      AmountToBill: BillingAmount }

and OrderLine =
    { Id: OrderLineId
      OrderId: OrderId
      ProductCode: ProductCode
      Quantity: OrderQuantity
      Price: Price }

type UnvalidatedOrder =
    { OrderId: string
      CustomerInfo: string
      ShippingAddress: string }

type PlaceOrderEvents =
    { AcknowledgementSent: Undefined
      OrderPlaced: Undefined
      BillableOrderPlaced: Undefined }

type PlaceOrderError =
    | ValidationError of ValidationError list
    | ProductNotFoundError of ProductCode

and ValidationError =
    { FieldName: string
      ErrorDescription: string }

// Domain - "verbs"

type PlaceOrder = UnvalidatedOrder -> Result<PlaceOrderEvents, PlaceOrderError>

module Order =
    let findOrderLine orderLineId orderLines =
        orderLines |> List.find (fun line -> line.Id = orderLineId)

    let replaceOrderLine orderLineId newOrderLine orderLines =
        orderLines
        |> List.map (fun line -> if line.Id = orderLineId then newOrderLine else line)

    let changeOrderLinePrice order orderLineId newPrice =
        let orderLine = order.OrderLines |> findOrderLine orderLineId
        let newOrderLine = { orderLine with Price = newPrice }
        let newOrderLines = order.OrderLines |> replaceOrderLine orderLineId newOrderLine

        let newAmountToBill = newOrderLines |> List.sumBy (fun line -> line.Price)

        let newOrder =
            { order with
                OrderLines = newOrderLines
                AmountToBill = newAmountToBill }

        newOrder

module EmailVerificationService =
    type VerifiedEmailAddress = private Email of string
    type verify = UnverifiedEmailAddress -> Result<VerifiedEmailAddress, string>

    let getEmail = fun (Email email) -> email

    let verifyEmail: UnverifiedEmailAddress -> Result<VerifiedEmailAddress, string> =
        fun email ->
            match createVerifiedEmailAddress email with
            | Some email -> Ok email
            | None -> Error "Invalid email address"
