object CaesarCipher extends App{

    val alphaUpper= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val alphaLower= "abcdefghijklmnopqrstuvwxyz"

    //Encryption
    def Encrypt(text:String, key:Int)=text.map{
        
        case c if alphaUpper.contains(c) => cipherU(alphaUpper, c, key)     //uppercase
        case c if alphaLower.contains(c) => cipherL(alphaLower, c, key)     // lowercase
        case c => c
    }

    //Decryption
    def Decrypt(text:String, key:Int)={
        val reverse:Int = -key
        Encrypt(text,reverse)
    }

    def cipherU(template:String, c:Char, key:Int)={
        alphaUpper((c-65+key+template.size) % alphaUpper.size)
    }

    def cipherL(template:String, c:Char, key:Int)={
        alphaLower((c-97+key+template.size) % alphaLower.size)
    }

    //val key= scala.io.StdIn.readLine("Key is: ").toInt
    val key=3
    //val text="I am on THE way"
    val text= scala.io.StdIn.readLine("Enter text: ")
    val encoded=Encrypt(text, key)

    println("Plaintext               => " + text)
    println("Ciphertext is           => " + encoded)
    println("Decrypted Ciphertext is => " +Decrypt(encoded, key))

}
