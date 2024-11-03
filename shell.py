import dhokla

while True:
    text = input("Dhokla> ")
    result,error = dhokla.run('<stdin>',text)
    if error:print(error.as_string())
    else:print(result)

    #print(result,error.as_string())
