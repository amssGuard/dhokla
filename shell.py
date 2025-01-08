import dhokla 

while True:
    text = input('Dhokla> ')
    result,error = dhokla.run(text)
    #result,error = dhokla.run_with_vm(text)

    if error: print(error.as_string())
    elif result:print(result)
