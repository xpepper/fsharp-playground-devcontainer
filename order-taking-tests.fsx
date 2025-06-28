#load "order-taking.fsx"
open OrderTaking.Domain

// Simple assertion function
let assertEqual expected actual message =
    if expected <> actual then
        failwithf "%s: Expected %A but got %A" message expected actual

// Test function
let testValidateOrderWithValidInputs () =
    printfn "Running test: If product exists, validation succeeds"

    // Arrange: set up stub versions of service dependencies
    let checkAddressExists address = true // succeed
    let checkProductCodeExists productCode = true // succeed

    // Arrange: set up input
    let unvalidatedOrder: UnvalidatedOrder =
        { OrderId = "123"
          CustomerInfo = "John Doe"
          ShippingAddress = "123 Main St"
          BillingAddress = "456 Elm St"
          OrderLines =
            [ { OrderLineId = "1"
                ProductCode = "W1234"
                Quantity = 10 }
              { OrderLineId = "2"
                ProductCode = "G123"
                Quantity = 5 } ] }

    // Act: call validateOrder
    let result =
        examples.validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder

    // Assert: check specific properties to verify it's a ValidatedOrder
    assertEqual "123" (OrderId.value result.OrderId) "OrderId should match"
    assertEqual "John Doe" (CustomerInfo.value result.CustomerInfo) "CustomerInfo should match"
    assertEqual 2 (result.OrderLines.Length) "Should have 2 order lines"

    printfn "Test passed!"

// Run the test
let runTests () =
    testValidateOrderWithValidInputs ()
    printfn "All tests passed!"

// Execute tests
runTests ()
