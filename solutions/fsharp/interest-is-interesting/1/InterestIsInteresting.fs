module InterestIsInteresting

let interestRate (balance: decimal): single =

   if balance < 0m then 3.213f
   elif balance >= 5000m then 2.475f
   elif balance >= 1000m then 1.621f
   else 0.5f

let interest (balance: decimal): decimal =

   let interest = interestRate balance |> decimal
   balance * (interest / 100m)

let annualBalanceUpdate(balance: decimal): decimal =
   interest balance + balance

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =

   if balance > 0.0m
   then (balance * decimal (taxFreePercentage / 100.0 * 2.0)) |> int
   else 0
