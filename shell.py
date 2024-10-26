import dhokla

while True:
    text = input("Dhokla> ")
    result,error = dhokla.run(text)
    if error:print(error.as_string())
    else:print(result)

