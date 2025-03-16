namespace OrderTaking.Domain

open System

[<Measure>]
type kg

// Domain - "nouns"

type WidgetCode = WidgetCode of string // A wrapper type. Starting with "W" then 4 digits
type GizmoCode = GizmoCode of string // A wrapper type. Starting with "G" then 3 digits

type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

module ProductCode =
    let create (code: string) =
        if code.StartsWith("W") && code.Length = 5 then
            Widget(WidgetCode code)
        elif code.StartsWith("G") && code.Length = 4 then
            Gizmo(GizmoCode code)
        else
            failwith "Invalid product code"

type UnitQuantity = private UnitQuantity of int

module UnitQuantity =
    // Define a "smart constructor" for UnitQuantity​​
    // int -> Result<UnitQuantity,string>
    let create quantity =
        if quantity < 1 then
            failwith "Quantity must be at least 1"
        elif quantity > 1000 then
            failwith "Quantity must be at most 1000"
        else
            UnitQuantity quantity

    let value (UnitQuantity quantity) = quantity

type KilogramQuantity = KilogramQuantity of decimal<kg> // A wrapper type. Between 0.05 and 100.0

module KilogramQuantity =
    let create quantity =
        if quantity < 0.05M<kg> then
            failwith "Quantity must be at least 0.05 kg"
        elif quantity > 100.0M<kg> then
            failwith "Quantity must be at most 100.0 kg"
        else
            KilogramQuantity quantity

type OrderQuantity =
    | Units of UnitQuantity
    | Kilograms of KilogramQuantity

type Undefined = exn

type OrderId = private OrderId of string

module OrderId =
    /// Define a "Smart constructor" for OrderId
    let create str =
        if String.IsNullOrEmpty(str) then
            // use exceptions rather than Result for now
            failwith "OrderId must not be null or empty"
        elif str.Length > 50 then
            failwith "OrderId must not be more than 50 chars"
        else
            OrderId str

    /// Extract the inner value from an OrderId
    let value (OrderId str) = // unwrap in the parameter!
        str

type OrderLineId = private OrderLineId of string

module OrderLineId =
    let create str =
        if String.IsNullOrEmpty(str) then
            failwith "OrderLineId must not be null or empty"
        elif str.Length > 50 then
            failwith "OrderLineId must not be more than 50 chars"
        else
            OrderLineId str

    let value (OrderLineId str) = str

type CustomerId = Undefined

type CustomerInfo = Undefined
type ShippingAddress = Undefined
type BillingAddress = Undefined
type Price = Undefined
type BillingAmount = BillingAmount of decimal

module BillingAmount =
    let value (BillingAmount amount) = amount

type NonEmptyList<'a> = { First: 'a; Rest: 'a list }

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

type UnvalidatedOrderLine =
    { OrderLineId: string
      ProductCode: string
      Quantity: int }

type ValidatedOrderLine =
    { OrderLineId: OrderLineId
      ProductCode: ProductCode
      Quantity: OrderQuantity }

type UnvalidatedOrder =
    { OrderId: string
      CustomerInfo: string
      ShippingAddress: string
      BillingAddress: string
      ProductCode: ProductCode }

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


type PricedOrderLine = Undefined

type PricedOrder =
    { OrderId: OrderId
      CustomerInfo: CustomerInfo
      ShippingAddress: ShippingAddress
      BillingAddress: BillingAddress
      OrderLines: PricedOrderLine list
      AmountToBill: BillingAmount }

type CheckProductCodeExists = ProductCode -> bool
type CheckAddressExists = string -> bool

type HtmlString = HtmlString of string
type EmailAddress = EmailAddress of string

type OrderAcknowledgment =
    { EmailAddress: EmailAddress
      Letter: HtmlString }

type OrderPlaced = PricedOrder

type BillableOrderPlaced =
    { OrderId: OrderId
      BillingAddress: BillingAddress
      AmountToBill: BillingAmount }

type SendResult =
    | Sent
    | NotSent

type SendOrderAcknowledgment = OrderAcknowledgment -> SendResult

type OrderAcknowledgmentSent =
    { OrderId: OrderId
      EmailAddress: EmailAddress }

type PlaceOrderEvent =
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced
    | AcknowledgmentSent of OrderAcknowledgmentSent

type CreateEvents = PricedOrder -> OrderAcknowledgmentSent option -> PlaceOrderEvent list

module examples =
    let predicateToPassthru errorMessage f x =
        if f x then x else failwith errorMessage

    let toProductCode (checkProductCodeExists: CheckProductCodeExists) productCode =
        let checkProduct: ProductCode -> ProductCode =
            let errorMsg = sprintf "Invalid: %A" productCode
            predicateToPassthru errorMsg checkProductCodeExists

        productCode |> ProductCode.create |> checkProduct

    let toOrderQuantity productCode quantity =
        match productCode with
        | Widget _ ->
            quantity
            |> int // convert decimal to int
            |> UnitQuantity.create // to UnitQuantity
            |> OrderQuantity.Units // lift to OrderQuantity type
        | Gizmo _ ->
            quantity
            |> decimal // convert int to decimal
            |> (*) 1.0M<kg> // convert decimal to decimal<kg>
            |> KilogramQuantity.create // to KilogramQuantity
            |> OrderQuantity.Kilograms // lift to OrderQuantity type


    let toValidatedOrderLine checkProductCodeExists (unvalidatedOrderLine: UnvalidatedOrderLine) =
        let orderLineId = unvalidatedOrderLine.OrderLineId |> OrderLineId.create

        let productCode =
            unvalidatedOrderLine.ProductCode |> toProductCode checkProductCodeExists // helper function

        let quantity = unvalidatedOrderLine.Quantity |> toOrderQuantity productCode // helper function

        let validatedOrderLine: ValidatedOrderLine =
            { OrderLineId = orderLineId
              ProductCode = productCode
              Quantity = quantity }

        validatedOrderLine

    type ValidateOrder =
        CheckProductCodeExists -> CheckAddressExists -> UnvalidatedOrder -> Result<Order, ValidationError list>

    let validateOrder: ValidateOrder =
        fun checkProductCodeExists checkAddressExists unvalidatedOrder -> failwith "not implemented"

    let createBillingEvent (placedOrder: PricedOrder) : BillableOrderPlaced option =
        let billingAmount = placedOrder.AmountToBill |> BillingAmount.value

        if billingAmount > 0M then
            let order =
                { OrderId = placedOrder.OrderId
                  BillingAddress = placedOrder.BillingAddress
                  AmountToBill = placedOrder.AmountToBill }

            Some order
        else
            None

    let createEvents: CreateEvents =
        fun pricedOrder acknowledgmentEventOpt ->
            let events1 = pricedOrder |> PlaceOrderEvent.OrderPlaced |> List.singleton

            let events2 =
                acknowledgmentEventOpt
                |> Option.map PlaceOrderEvent.AcknowledgmentSent
                |> Option.toList

            let events3 =
                pricedOrder
                |> createBillingEvent
                |> Option.map PlaceOrderEvent.BillableOrderPlaced
                |> Option.toList

            [ yield! events1; yield! events2; yield! events3 ]
