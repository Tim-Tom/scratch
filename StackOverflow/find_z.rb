def nearby_az(string)
  last_a = -4
  string.chars.each_with_index do |c, i|
    last_a = i if c == 'a'
    return true if c == 'z' and i - last_a <= 3
  end
  return false
end

puts nearby_az("aaaaabcdz")
