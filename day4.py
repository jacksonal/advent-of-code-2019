def HasAdjacentValues(number):
    while number//10 > 0:
        ones = number % 10
        number = number//10
        
        if ones == number % 10:
            return True
    return False
        
def HasGroupOfTwoAdjacentValues(number):
    groupSize = 1
    while number//10 > 0:
        ones = number % 10
        number = number//10
        if ones == number % 10:
            groupSize+=1
        elif groupSize == 2:
            return True
        else:
            groupSize = 1
    return groupSize == 2

def HasDecreasingDigits(number):
    while number//10 > 0:
        ones = number % 10
        number = number//10
        if ones < number % 10:
            return False
    return True

if __name__ == "__main__":
    count = 0
    for num in range(183564,657475):
        if HasGroupOfTwoAdjacentValues(num) and HasDecreasingDigits(num):
            count = count + 1
    print(count)
