# IEEE754-Helper
Little CLI Tool to help convert IEEE754 compliant numbers.

Just download the [current release](https://github.com/ChrisgammaDE/IEEE754-Helper/releases) and try it out!


## Help

````
IEEE Helper Tool                                                                                                
                                                                                                                
Usage:                                                                                                          
  ieee-helper BinToHex <binNumber>                                                                              
  ieee-helper HexToBin <hexNumber>                                                                              
  ieee-helper DecToBin <precision> <decNumber>                                                                  
  ieee-helper BinToDec <binNumber>                                                                              
  ieee-helper DecToIEEE [(Single|s)|(Double|d)] <decNumber>                                                     
  ieee-helper IEEEToDec <ieee754BinNumber>                                                                      
  ieee-helper -h | --help                                                                                       
  ieee-helper -v | --version                                                                                    
                                                                                                                
Options:                                                                                                        
  -h --help             Show this help screen                                                                   
  -v --version          Show current Version                                                                    
                                                                                                                
Command Descriptions:                                                                                           
  BinToHex <binNumber>                          Convert a binary number to a hexadecimal number.                
  HexToBin <hexNumber>                          Convert a hexadecimal number to a binary number.                
  DecToBin <precision> <decNumber>              Convert a decimal number to a binary number.                    
                                                  Precision is the number of bits after the point.              
  BinToDec <binNumber>                          Convert binary floating number to decimal number.               
  DecToIEEE [(Single|s)|(Double|d)] <decNumber> Convert a decimal number to a IEEE754 compliant number.         
  IEEEToDec <ieee754BinNumber>                  Input a IEEE754 binary number and get the decimal representation.                                                                                                               
                                                                                                                
Argument Descriptions:                                                                                          
  binaryNumber          Binary number. Don't seperate by spaces. E.g. 0100101101                                
  hexNumber             Hexadecimal number. E.g. 0x42DF                                                         
  precision             How many digits after the point should be calculated? E.g. 3                            
  decNumber             Floating point decimal number. E.g. 3.14                                                
  ieee754BinNumber      IEEE754 compliant bin number. E.g. 01000000010010001111010111000010                     
                                                                                                                
Examples:                                                                                                       
  ieee-helper HexToBin 0x43D.54                                                                                 
  ieee-helper DecToBin 23 3.1416                                                                                
  ieee-helper DecToIEEE Single 42.743                                                                           
  ieee-helper IEEEToDec 0x42b844dd                                                                              
  ieee-helper BinToHex 110110.1011001 
  
 ````
 
