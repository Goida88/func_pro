let start = 0.0
let finish = 0.5
let steps = 10.0
let precision = 0.001

let func x = log((1.0 + x) / (1.0 - x))

let rec iterate idx limit applyFunc x =
  if idx <= limit then
    let newX = applyFunc x
    iterate (idx + 1.0) limit applyFunc newX
  else
    x

let multiply a b = a * b

let power base exp = iterate 1.0 exp (multiply base) base

let rec taylorSeries x iteration precisionValue partialSum = 
  let term1 = (power x (2.0 * iteration + 1.0)) / (2.0 * iteration + 1.0)
  let term2 = (power x (2.0 * (iteration + 1.0) + 1.0)) / (2.0 * (iteration + 1.0) + 1.0)
  if abs(term2 - term1) < precisionValue then
    (2.0 * (partialSum + term1), iteration)
  else
    let newSum = partialSum + term1
    taylorSeries x (iteration + 1.0) precisionValue newSum

let rec optimizedTaylor x prevTerm iteration precisionValue partialSum = 
  let nextTerm = power x 2.0 * prevTerm * (2.0 * (iteration - 1.0) + 1.0) / (2.0 * iteration + 1.0)
  if abs(nextTerm - prevTerm) < precisionValue then
    (2.0 * partialSum, (iteration - 1.0) / 2.0)
  else
    let newSum = partialSum + nextTerm
    optimizedTaylor x nextTerm (iteration + 2.0) precisionValue newSum

let calculateValue i = 
  let x = start + (float i) / (float steps) * (finish - start)
  let naiveResult, naiveIters = taylorSeries x 0.0 precision 0.0
  let smartResult, smartIters = optimizedTaylor x x 1.0 precision x
  printfn "|%5.2f|  %10.6f|  %10.6f|   %10.0f|  %10.6f|   %10.0f|" x (func x) naiveResult naiveIters smartResult smartIters
  i + 1.0

let executeMain =
    printfn "--------------------------------------------------------------------------"
    printfn "|  x  |    f(x)    |   Naive    |    Iters    |   Smart    |    Iters    |"
    printfn "--------------------------------------------------------------------------"
    iterate 0.0 steps calculateValue 0.0
    printfn "--------------------------------------------------------------------------"

executeMain
