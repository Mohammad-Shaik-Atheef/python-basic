import random
x=random.randint(0,20)
trail=0
while  True:
    a = int(input("what is your number:\n>> "))
    if trail==3:
        print("your attempts are completed")
        break

    elif a>x:
        print("number is too high ")
        trail=trail+1
    elif a<x:
        print("number is too small")
        trail=trail+1
    else :
        print("you are right")
        break






