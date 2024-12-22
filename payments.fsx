type CheckNumber = CheckNumber of int
type CreditCardNumber = CreditCardNumber of string
type Amount = Amount of decimal

type Currency =
    | EUR
    | DOLLAR
    | YUAN

type Issuer =
    | Visa
    | Mastercard

type Month = 
  | Jan
  | Feb
  | Dec

type Year = Year of int

type ExpirationDate = {
    Month: Month
    Year: Year
}

type CreditCardInfo =
    { CreditCardNumber: CreditCardNumber
      Issuer: Issuer 
      ExpirationDate: ExpirationDate
      }

type PaymentMethod =
    | Cash
    | Check of CheckNumber
    | CreditCard of CreditCardInfo

type Payment =
    { Method: PaymentMethod
      Amount: Amount
      Currency: Currency }


let tenEurosViaCreditCard =
    { Method =
        CreditCard
            { CreditCardNumber = CreditCardNumber "1234-5678-9012-3456"
              Issuer = Visa 
              ExpirationDate = { Month = Jan; Year = Year 2022 }
              }
      Amount = Amount 10m
      Currency = EUR }

type UnpaidInvoice = { InvoiceId: int; Amount: Amount }

type PaidInvoice =
    { InvoiceId: int
      Amount: Amount
      PaymentMethod: PaymentMethod
      PaidOn: System.DateTime }

type PaymentError =
    | CardTypeNotRecognized
    | PaymentRejected
    | InsufficientFunds
    | InvalidPaymentMethod

type PayInvoice = UnpaidInvoice -> PaymentMethod -> Result<PaidInvoice, PaymentError>

// type alias

let payInvoice: PayInvoice =
    fun invoice paymentMethod ->
        match paymentMethod with
        | CreditCard { CreditCardNumber = _
                       Issuer = Mastercard } ->
            { InvoiceId = invoice.InvoiceId
              Amount = invoice.Amount
              PaymentMethod = paymentMethod
              PaidOn = System.DateTime.Now }
            |> Ok
        | _ -> Error CardTypeNotRecognized

let invoiceToPay = { InvoiceId = 1; Amount = Amount 10m }

let aPaymentMethod =
    CreditCard
        { CreditCardNumber = CreditCardNumber "1234-5678-9012-3456"
          Issuer = Mastercard
          ExpirationDate = { Month = Jan; Year = Year 2022 }
          }

let paidInvoice = payInvoice invoiceToPay aPaymentMethod

let printPaidInvoce : PaidInvoice -> unit =
    fun paidInvoice ->
        printfn "Invoice %d paid on %A" paidInvoice.InvoiceId paidInvoice.PaidOn


type User =
    { Name: string
      MiddleName: Option<string>
      Surname: string
      Email: string }

let user =
    { Name = "Piero"
      MiddleName = None
      Surname = "Di Bello"
      Email = "piero@gmail.com" }

let anotherUser =
    { Name = "Paolo"
      MiddleName = Some "Nusco"
      Surname = "Perrotta"
      Email = "paolo@gmail.com" }


user.MiddleName |> printfn "%A"

type SaveUser = User -> unit

type NextRandom = unit -> int
let nextRandom: NextRandom = fun () -> 42

type OrderId = OrderId of int

type OrderLine = { ProductId: int; Quantity: int }

type Order =
    { OrderId: OrderId
      OrderLines: OrderLine list }

let anOrder =
    { OrderId = OrderId 1
      OrderLines = [ { ProductId = 1; Quantity = 2 }; { ProductId = 2; Quantity = 2 } ] }

let lines = [ { ProductId = 1; Quantity = 2 }; { ProductId = 2; Quantity = 2 } ]

let moreLines = { ProductId = 1; Quantity = 2 } :: { ProductId = 2; Quantity = 2 } :: []
