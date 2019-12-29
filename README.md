
# Network Address Representations

This library provides types to represent network addresses of various
kinds (e.g., IPv4, IPv6) as well as functions to print those values in
their canonical external format, and parse address values.

## Representation

 - *Type* `ipv4-address`
 
   Instances of this type represent IPv4 network addresses. The canonical
   representation are four decimal octet values separated by dots `x.x.x.x`.
   The CPL is `ipv4-address`, `structure-object`, `t`.
 
 - *Generic Function* `ipv4-address` _value_ &rarr; _address_
 
   Ensures, that _value_ is an instance of type `ipv4-address` coercing it
   when necessary (and possible.) If _value_ is neither an IPv4 address value
   nor can it be coerced, this function signals a `type-error` condition.
   
   The following methods are pre-defined by the library
   
   - *Method* `ipv4-address` (_value_ `ipv4-address`) &rarr; _address_
   - *Method* `ipv4-address` (_value_ `string`) &rarr; _address_
   - *Method* `ipv4-address` (_value_ `array`) &rarr; _address_
   - *Method* `ipv4-address` (_value_ `integer`) &rarr; _address_
   - *Method* `ipv4-address` (_value_ `ipv6-address`) &rarr; _address_
 
 - *Function* `ipv4-address-p` _value_ &rarr; _boolean_
 
   Answers true, if _value_ is an instance of class `ipv4-address`.
 
 - *Type* `ipv6-address`

   Instances of this type represent IPv6 network addresses. The canonical
   representation are 8 limbs of up to four hexadecimal digits, separated by
   colons: `x:x:x:x:x:x:x:x` (and subject to further canonicalization rules.)
   The CPL is `ipv6-address`, `structure-object`, `t`.

 - *Generic Function* `ipv6-address` _value_ &rarr; _address_

   Ensures, that _value_ is an instance of type `ipv6-address` coercing it
   when necessary (and possible.) If _value_ is neither an IPv6 address value
   nor can it be coerced, this function signals a `type-error` condition.
   
   The following methods are pre-defined by the library
   
   - *Method* `ipv6-address` (_value_ `ipv6-address`) &rarr; _address_
   - *Method* `ipv6-address` (_value_ `string`) &rarr; _address_
   - *Method* `ipv6-address` (_value_ `array`) &rarr; _address_
   - *Method* `ipv6-address` (_value_ `integer`) &rarr; _address_
   - *Method* `ipv6-address` (_value_ `ipv4-address`) &rarr; _address_

 - *Function* `ipv6-address-p` _value_ &rarr; _boolean_
 
   Answers true, if _value_ is an instance of class `ipv6-address`.
 
## Comparing Addresses And Hashing

 - *Generic Function* `address=` _value1_ _value2_ &rarr; _boolean_
 
 - *Generic Function* `address<` _value1_ _value2_ &rarr; _boolean_
 
 - *Function* `address<=` _value1_ _value2_ &rarr; _boolean_
 
 - *Function* `address>=` _value1_ _value2_ &rarr; _boolean_
 
 - *Function* `address/=` _value1_ _value2_ &rarr; _boolean_
 
 - *Function* `address>` _value1_ _value2_ &rarr; _boolean_
 
 - *Generic Function* `address-hash` _address_ &rarr; _integer_
 
## Printing And Parsing

 - *Generic Function* `print-address` _value_ _stream_ `&key` &rarr; _undefined_
 
 - *Function* `parse-ipv4-address` _string_ `&key` _start_ _end_ _junk-allowed_ &rarr; _address_
 
 - *Function* `parse-ipv6-address` _string_ `&key` _start_ _end_ _junk-allowed_ &rarr; _address_
 
 - *Function* `address-string` _address_ `&rest` _options_ &rarr; _string_

## Other Operations

 - *Generic Function* `address-bytes` _value_ &rarr; _array_
 
 
