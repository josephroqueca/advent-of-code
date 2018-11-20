input = nil

if File.basename(Dir.getwd).match?(/day\d\d$/) then
    input = File.readlines('input.txt')
else
    input = File.readlines('day01/input.txt')
end

input = input[0]

sum = 0
input.split("").each_with_index do |c, i|
    if c == input[(i + input.length / 2) % input.length] then
        sum += c.to_i
    end
end

puts sum