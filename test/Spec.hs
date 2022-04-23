import Test.Hspec
import Process
import Channel
import PiQQ
import Examples (piFac, piFacRec, piCollatz, piCollatzRec, piHanoi, piHanoiRec, piFib, piFibRec, piQuicksort, piQuicksortRec)
import PrivateMVar
-- import GlobalMVar
-- import PrivateTMVar
-- import GlobalTMVar
-- import Async

main :: IO ()
main = hspec do
  describe "piFac" do
    it "computes the factorial of 20" do
      runProcess piFac `shouldReturn` fac 20

  describe "piFacRec" do
    it "computes the factorial of 20" do
      runProcess piFacRec `shouldReturn` fac 20
  
  describe "piCollatz" do
    it "computes how many steps it takes for number 989345275647 to reach 1" do
      runProcess piCollatz `shouldReturn` collatz 989345275647

  describe "piCollatzRec" do  
    it "computes how many steps it takes for number 989345275647 to reach 1" do
      runProcess piCollatzRec `shouldReturn` collatz 989345275647
  
  describe "piHanoi" do
    it "computes how many steps a hanoi game with 16 discs takes" do
      runProcess piHanoi `shouldReturn` hanoi 16
    
  describe "piHanoiRec" do
    it "computes how many steps a hanoi game with 16 discs takes" do
      runProcess piHanoiRec `shouldReturn` hanoi 16
  
  describe "piFib" do
    it "computes the fibonacci number of 15" do
      runProcess piFib `shouldReturn` fib 15
    
  describe "piFibRec" do
    it "computes the fibonacci number of 15" do
      runProcess piFibRec `shouldReturn` fib 15
  
  describe "piQuicksort" do
    it "sorts a list using the quicksort algorithm" do
      runProcess (piQuicksort l) `shouldReturn` quicksort l
    
  describe "piQuicksortRec" do
    it "sorts a list using the quicksort algorithm" do
      runProcess (piQuicksortRec l) `shouldReturn` quicksort l


l :: [Int]
l = [232, 14, 335, 9, 280, 56, 79, 87, 306, 479, 426, 411, 499, 260, 12, 373, 52, 322, 123, 461, 331, 407, 298, 384, 402, 423, 429, 494, 183, 15, 185, 150, 99, 393, 124, 303, 337, 113, 290, 294, 50, 4, 110, 285, 78, 139, 403, 457, 75, 131, 475, 38, 201, 361, 266, 356, 308, 493, 73, 112, 374, 167, 364, 416, 31, 405, 332, 128, 69, 80, 205, 304, 474, 348, 414, 389, 390, 182, 486, 43, 478, 18, 477, 57, 142, 46, 257, 235, 158, 125, 17, 277, 302, 26, 215, 388, 340, 19, 472, 238, 330, 443, 466, 157, 171, 45, 42, 111, 287, 305, 409, 228, 249, 272, 134, 108, 40, 200, 29, 283, 186, 211, 496, 372, 397, 168, 359, 460, 231, 398, 386, 419, 189, 483, 191, 217, 118, 333, 381, 312, 345, 155, 261, 97, 239, 444, 497, 35, 41, 208, 391, 282, 130, 378, 286, 268, 375, 89, 51, 489, 137, 324, 255, 198, 344, 222, 383, 59, 481, 102, 61, 476, 16, 467, 334, 352, 488, 187, 284, 270, 484, 415, 262, 246, 458, 436, 355, 432, 253, 241, 82, 71, 435, 3, 438, 341, 229, 417, 94, 288, 247, 164, 366, 27, 455, 387, 213, 485, 234, 487, 54, 500, 83, 225, 66, 441, 310, 318, 32, 116, 197, 65, 273, 382, 227, 223, 6, 96, 240, 440, 135, 269, 195, 431, 354, 463, 245, 173, 276, 220, 93, 369, 311, 447, 30, 129, 448, 349, 254, 498, 24, 347, 274, 138, 446, 115, 301, 178, 454, 122, 62, 37, 210, 109, 207, 161, 86, 63, 152, 49, 48, 2, 464, 101, 317, 224, 226, 77, 371, 252, 263, 81, 289, 275, 406, 420, 495, 401, 176, 58, 471, 408, 194, 442, 299, 376, 72, 396, 451, 22, 365, 323, 55, 327, 204, 106, 433, 74, 144, 68, 380, 92, 165, 314, 209, 212, 154, 315, 480, 313, 490, 188, 136, 292, 233, 180, 117, 346, 39, 145, 193, 190, 172, 351, 184, 412, 179, 343, 430, 462, 422, 321, 293, 258, 202, 439, 360, 424, 358, 177, 95, 7, 119, 320, 25, 162, 328, 181, 470, 385, 492, 160, 291, 473, 465, 491, 23, 169, 437, 418, 11, 60, 296, 400, 295, 256, 368, 325, 353, 218, 357, 121, 107, 64, 103, 166, 133, 410, 148, 214, 33, 309, 13, 379, 264, 250, 399, 120, 20, 434, 482, 281, 88, 329, 192, 146, 350, 10, 104, 469, 196, 199, 8, 70, 316, 5, 394, 319, 53, 427, 44, 84, 230, 251, 90, 91, 105, 98, 450, 297, 159, 279, 367, 170, 174, 153, 445, 219, 248, 36, 242, 362, 28, 421, 449, 147, 47, 425, 271, 307, 236, 452, 21, 243, 336, 206, 126, 132, 300, 156, 338, 175, 428, 326, 34, 85, 468, 404, 216, 140, 203, 143, 392, 278, 221, 100, 377, 163, 370, 76, 259, 141, 267, 1, 149, 453, 395, 67, 342, 413, 459, 114, 339, 151, 237, 363, 456, 265, 127, 244]

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

collatz :: Integer -> Integer
collatz n = collatz' n 0
  where
    collatz' :: Integer -> Integer -> Integer
    collatz' 1 steps = steps
    collatz' n steps
      | even n = collatz' (n `div` 2) (steps+1)
      | otherwise = collatz' (3*n+1) (steps+1)

hanoi :: Int -> Integer
hanoi 1 = 1
hanoi n = 2 * hanoi (n-1) + 1

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

partition :: Ord a => (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition pred (x:xs)
  | pred x = (x:r1, r2)
  | otherwise = (r1, x:r2)
  where
    (r1, r2) = partition pred xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = let (less, greater) = partition (<x) xs
                    in quicksort less ++ (x : quicksort greater)
