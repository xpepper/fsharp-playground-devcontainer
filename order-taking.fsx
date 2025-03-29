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

    let value (KilogramQuantity quantity) = quantity

type OrderQuantity =
    | Units of UnitQuantity
    | Kilograms of KilogramQuantity

module OrderQuantity =
    let value quantity =
        match quantity with
        | Units unitQuantity -> UnitQuantity.value unitQuantity |> decimal
        | Kilograms kilogramQuantity -> KilogramQuantity.value kilogramQuantity |> decimal

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

type CustomerInfo = CustomerInfo of string

module CustomerInfo =
    let create str = CustomerInfo str
    let value (CustomerInfo str) = str
type ShippingAddress = ShippingAddress of string
type BillingAddress = BillingAddress of string

module ShippingAddress =
    let create str = ShippingAddress str
    let value (ShippingAddress str) = str

module BillingAddress =
    let create str = BillingAddress str
    let value (BillingAddress str) = str
type Price = Price of decimal

module Price =
    let create price = Price price
    let multiply qty (Price p) = create (qty * p)

    let value (Price price) = price

type BillingAmount = BillingAmount of decimal

module BillingAmount =
    let value (BillingAmount amount) = amount

    let create amount = BillingAmount amount

    let sumPrices prices =
        let total = prices |> List.map Price.value |> List.sum
        create total

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
      OrderLines: UnvalidatedOrderLine list }

type PlaceOrderEvents =
    { AcknowledgementSent: Undefined
      OrderPlaced: Undefined
      BillableOrderPlaced: Undefined }

type PricingError =
    | ProductNotFoundError of ProductCode
    | InvalidOrderLineError of OrderLineId

type PlaceOrderError =
    | Validation of ValidationError
    | Pricing of PricingError

// type PlaceOrderError =
//     | ValidationError of ValidationError list
//     | ProductNotFoundError of ProductCode
and ValidationError =
    { FieldName: string
      ErrorDescription: string }

// Domain - "verbs"

// type PlaceOrder = UnvalidatedOrder -> Result<PlaceOrderEvents, PlaceOrderError>
type PlaceOrder = UnvalidatedOrder -> PlaceOrderEvents

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

type UnverifiedEmailAddress = UnverifiedEmailAddress of string

module EmailVerificationService =
    type VerifiedEmailAddress = private Email of string
    type verify = UnverifiedEmailAddress -> Result<VerifiedEmailAddress, string>

    let getEmail = fun (Email email) -> email

    let createVerifiedEmailAddress (UnverifiedEmailAddress email) =
        if email.Contains("@") then Some(Email email) else None

    let verifyEmail: UnverifiedEmailAddress -> Result<VerifiedEmailAddress, string> =
        fun email ->
            match createVerifiedEmailAddress email with
            | Some email -> Ok email
            | None -> Error "Invalid email address"


type PricedOrderLine =
    { OrderLineId: OrderLineId
      ProductCode: ProductCode
      Quantity: OrderQuantity
      LinePrice: Price }

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

type ValidatedOrder =
    { OrderId: OrderId
      CustomerInfo: CustomerInfo
      ShippingAddress: ShippingAddress
      BillingAddress: BillingAddress
      OrderLines: ValidatedOrderLine list }

type GetProductPrice = ProductCode -> Price

// type PriceOrder = GetProductPrice -> ValidatedOrder -> PricedOrder
type PriceOrder = GetProductPrice -> ValidatedOrder -> Result<PricedOrder, PricingError>

// type PlaceOrderWorkflow = PlaceOrder -> PlaceOrderEvent list
type PlaceOrderWorkflow = UnvalidatedOrder -> PlaceOrderEvent list

type CreateOrderAcknowledgmentLetter = PricedOrder -> HtmlString

type AcknowledgeOrder =
    CreateOrderAcknowledgmentLetter -> SendOrderAcknowledgment -> PricedOrder -> OrderAcknowledgmentSent option

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
        CheckProductCodeExists -> CheckAddressExists -> UnvalidatedOrder -> Result<ValidatedOrder, ValidationError>

    let validateOrder: ValidateOrder =
        fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
            let orderId = unvalidatedOrder.OrderId |> OrderId.create
            let customerInfo = unvalidatedOrder.CustomerInfo |> CustomerInfo.create
            let shippingAddress = unvalidatedOrder.ShippingAddress |> ShippingAddress.create
            let billingAddress = unvalidatedOrder.BillingAddress |> BillingAddress.create

            let validatedOrderLines =
                unvalidatedOrder.OrderLines
                |> List.map (toValidatedOrderLine checkProductCodeExists)

            let amountToBill =
                validatedOrderLines
                |> List.map (fun line -> line.Quantity |> OrderQuantity.value)
                |> List.sumBy (fun qty -> qty)

                // add them together as a BillingAmount
                |> BillingAmount.create

            let validatedOrder: ValidatedOrder =
                { OrderId = orderId
                  CustomerInfo = customerInfo
                  ShippingAddress = shippingAddress
                  BillingAddress = billingAddress
                  OrderLines = validatedOrderLines }

            Ok validatedOrder

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

    let acknowledgeOrder: AcknowledgeOrder =
        fun createAcknowledgmentLetter sendAcknowledgment pricedOrder ->
            let letter = createAcknowledgmentLetter pricedOrder

            let acknowledgment =
                { EmailAddress = pricedOrder.CustomerInfo.EmailAddress
                  Letter = letter }
            // if the acknowledgment was successfully sent,
            // return the corresponding event, else return None
            match sendAcknowledgment acknowledgment with
            | Sent ->
                let event =
                    { OrderId = pricedOrder.OrderId
                      EmailAddress = pricedOrder.CustomerInfo.EmailAddress }

                Some event
            | NotSent -> None

    let toPricedOrderLine getProductPrice (line: ValidatedOrderLine) : PricedOrderLine =
        let qty = line.Quantity |> OrderQuantity.value
        let price = line.ProductCode |> getProductPrice
        let linePrice = price |> Price.multiply qty

        { OrderLineId = line.OrderLineId
          ProductCode = line.ProductCode
          Quantity = line.Quantity
          LinePrice = linePrice }

    let priceOrder: PriceOrder =
        fun getProductPrice validatedOrder ->
            let lines =
                validatedOrder.OrderLines |> List.map (toPricedOrderLine getProductPrice)

            let amountToBill =
                lines
                |> List.map (fun line -> line.LinePrice)
                // add them together as a BillingAmount
                |> BillingAmount.sumPrices

            let pricedOrder: PricedOrder =
                { OrderId = validatedOrder.OrderId
                  CustomerInfo = validatedOrder.CustomerInfo
                  ShippingAddress = validatedOrder.ShippingAddress
                  BillingAddress = validatedOrder.BillingAddress
                  OrderLines = lines
                  AmountToBill = amountToBill }

            Ok pricedOrder

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

    let withAcknowledgeOrder acknowledgeOrder (pricedOrder: PricedOrder) =
        let acknowledgment = acknowledgeOrder pricedOrder
        (pricedOrder, acknowledgment)

    let placeOrder
        checkProductCodeExists
        checkAddressExists
        getProductPrice
        createAcknowledgmentLetter
        sendAcknowledgment
        : PlaceOrderWorkflow =
        // set up local versions of the pipeline stages using partial application to bake-in dependencies
            let validateOrder = validateOrder checkProductCodeExists checkAddressExists
            let priceOrder = priceOrder getProductPrice

            let acknowledgeOrder =
                acknowledgeOrder createAcknowledgmentLetter sendAcknowledgment

            fun unvalidatedOrder ->
                unvalidatedOrder
                |> validateOrder
                |> priceOrder
                |> withAcknowledgeOrder acknowledgeOrder
                |> fun (pricedOrder, acknowledgment) -> createEvents pricedOrder acknowledgment

    // our "composition root" function
    let checkProductCodeExists: CheckProductCodeExists = fun _ -> true
    let checkAddressExists: CheckAddressExists = fun _ -> true
    let getProductPrice: GetProductPrice = fun _ -> Price.create 1.0M

    let createAcknowledgmentLetter: CreateOrderAcknowledgmentLetter =
        fun pricedOrder ->
            let letter = sprintf "Thank you for your order %A" pricedOrder.OrderId
            HtmlString letter

    let sendAcknowledgment: SendOrderAcknowledgment =
        fun acknowledgment ->
            printfn "Sending acknowledgment to %A" acknowledgment.EmailAddress
            Sent

    let placeOrder =
        placeOrder
            checkProductCodeExists
            checkAddressExists
            getProductPrice
            createAcknowledgmentLetter
            sendAcknowledgment

    let validateOrderAdapted input=
        let validateOrder = validateOrder checkProductCodeExists checkAddressExists
        input
        |> validateOrder
        |> Result.mapError PlaceOrderError.Validation

    let priceOrderAdapted input =
        let priceOrder = priceOrder getProductPrice
        input
        |> priceOrder
        |> Result.mapError PlaceOrderError.Pricing

    let placeOrderAdapted unvalidatedOrder =
        let validateOrder = validateOrder checkProductCodeExists checkAddressExists
        let priceOrder = priceOrder getProductPrice

        let validateOrder = validateOrder >> Result.mapError PlaceOrderError.Validation
        let priceOrder = priceOrder >> Result.mapError PlaceOrderError.Pricing

        unvalidatedOrder
        |> validateOrder
        |> Result.bind priceOrder
        // ...

    placeOrder {
        OrderId = "123"
        CustomerInfo = "John Doe"
        ShippingAddress = "123 Main St"
        BillingAddress = "456 Elm St"
        OrderLines = [
            { OrderLineId = "1"; ProductCode = "W1234"; Quantity = 10 }
            { OrderLineId = "2"; ProductCode = "G123"; Quantity = 5 }
        ]
    }
    |> printfn "%A"
