# poker

https://scrapbox.io/prog-exercises/%E3%83%9D%E3%83%BC%E3%82%AB%E3%83%BC%E3%81%AE%E6%89%8B%E3%82%92%E3%81%99%E3%81%B9%E3%81%A6%E3%83%AA%E3%82%B9%E3%83%88%E3%81%99%E3%82%8B

```ruby
# ポーカーのあらゆる手をリストする
suits = ['S', 'D', 'H', 'C'] # ♠ ♦ ♥ ♣
numbers = ['A', '2', '3', '4', '5', '6', '7', '8', '9', 'X', 'J', 'Q', 'K']
(0..51).to_a.combination(5) { |a| # combinationという便利な関数がある
  puts a.collect { |i|
    suits[i%4] + numbers[i/4]
  }.join('')
}
```
