; @Harness: disassembler
; @Result: PASS
  section .text  size=0x00000020 vma=0x00000000 lma=0x00000000 offset=0x00000034 ;2**0 
  section .data  size=0x00000000 vma=0x00000000 lma=0x00000000 offset=0x00000054 ;2**0 

start .text:

label 0x00000000  ".text":
      0x0: 0x0f 0xef  ldi  r16,  0xFF  ;  255
      0x2: 0x1f 0xef  ldi  r17,  0xFF  ;  255
      0x4: 0x2f 0xef  ldi  r18,  0xFF  ;  255
      0x6: 0x3f 0xef  ldi  r19,  0xFF  ;  255
      0x8: 0x4f 0xef  ldi  r20,  0xFF  ;  255
      0xa: 0x5f 0xef  ldi  r21,  0xFF  ;  255
      0xc: 0x6f 0xef  ldi  r22,  0xFF  ;  255
      0xe: 0x7f 0xef  ldi  r23,  0xFF  ;  255
     0x10: 0x8f 0xef  ldi  r24,  0xFF  ;  255
     0x12: 0x9f 0xef  ldi  r25,  0xFF  ;  255
     0x14: 0xaf 0xef  ldi  r26,  0xFF  ;  255
     0x16: 0xbf 0xef  ldi  r27,  0xFF  ;  255
     0x18: 0xcf 0xef  ldi  r28,  0xFF  ;  255
     0x1a: 0xdf 0xef  ldi  r29,  0xFF  ;  255
     0x1c: 0xef 0xef  ldi  r30,  0xFF  ;  255
     0x1e: 0xff 0xef  ldi  r31,  0xFF  ;  255

start .data:
