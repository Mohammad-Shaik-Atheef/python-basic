import random

a=["rock","paper","scissor"]
b=random.choice(a)
print(b)
while True:
  c=input(str(print("it is rock,paper and scissor game \n lets start choose any one of it \n :")))
  if b=="rock" and c=="paper":
        print("you won")
  elif b=="rock" and c=="scissor":
        print("you lose")
  elif b=="scissor" and c=="paper":
        print("you won")
  elif b=="scissor" and c=="rock":
        print("you lose")
  elif b=="paper" and c=="rock":
        print("you lose")

  elif b == "paper" and c=="scissor":
        print("you won")
  else:
        print("draw")










