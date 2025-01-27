; Minimal example of using ca65 to build SNES ROM.

.p816   ; 65816 processor
.i16    ; X/Y are 16 bits
.a8     ; A is 8 bits

.include "snes.inc"
.include "charmap.inc"
.include "spritemap.inc"

.segment "HEADER"    ; +$7FE0 in file
.byte "Bubbles     " ; ROM name

.segment "ROMINFO"   ; +$7FD5 in file
.byte $30            ; LoROM, fast-capable
.byte 0              ; no battery RAM
.byte $07            ; 128K ROM
.byte 0,0,0,0
.word $AAAA,$5555    ; dummy checksum and complement

.segment "BSS"
oam_lo_buffer: .res 512
oam_hi_buffer: .res 32
oam_buffer_end:

.segment "CODE"
   jmp start

Joy1A          = $06CE
Joy1B          = $06CF
aim_obj        = $0000
aim_x           = $06D0
aim_y           = $06D1

BubbleStart = $0004
BubbleCount = $000A

VRAM_CHARSET   = $0000 ; must be at $1000 boundary
VRAM_BG1       = $1000 ; must be at $0400 boundary
VRAM_BG2       = $1400 ; must be at $0400 boundary
VRAM_BG3       = $1800 ; must be at $0400 boundary
VRAM_BG4       = $1C00 ; must be at $0400 boundary
START_X        = 5
START_Y        = 14
START_TM_ADDR  = VRAM_BG1 + 32*START_Y + START_X

hello_str: .asciiz "You can do it!"

KEY_B = $8000
KEY_Y = $4000
KEY_SELECT = $2000
KEY_START = $1000
KEY_UP = $0800
KEY_DOWN = $0400
KEY_LEFT = $0200
KEY_RIGHT = $0100
KEY_A = $0080
KEY_X = $0040
KEY_L = $0020
KEY_R = $0010

start:
   clc             ; native mode
   xce
   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit

   ; Clear registers
   ldx #$33

   stz aim_y

   jsr ClearVRAM
   jsr ClearOAM
   jsr CpyToDMA

@loop:
   stz INIDISP,x
   stz NMITIMEN,x
   dex
   bpl @loop

   lda #128
   sta INIDISP ; undo the accidental stz to 2100h due to BPL actually being a branch on nonnegative

   ; Set palette to black background and 3 shades of red
   stz CGADD ; start with color 0 (background)
   stz CGDATA ; None more black
   stz CGDATA
   lda #$10 ; Color 1: dark red
   sta CGDATA
   stz CGDATA
   lda #$1F ; Color 2: neutral red
   sta CGDATA
   stz CGDATA
   lda #$1F  ; Color 3: light red
   sta CGDATA
   lda #$42
   sta CGDATA
   
   ; Obj Pallet #$00
   lda #$80
   sta CGADD
   stz CGDATA  ; None
   stz CGDATA
   lda #$2B    ; Light Blue
   sta CGDATA
   lda #$73
   sta CGDATA

   lda #$6C    ; Dark Blue
   sta CGDATA
   lda #$7E
   sta CGDATA

   lda #$3f    ; Orange
   sta CGDATA
   lda #$12
   sta CGDATA

   ; Setup Graphics Mode 0, 8x8 tiles all layers
   stz BGMODE
   lda #>VRAM_BG1
   sta BG1SC ; BG1 at VRAM_BG1, only single 32x32 map (4-way mirror)
   lda #((>VRAM_CHARSET >> 4) | (>VRAM_CHARSET & $F0))
   sta BG12NBA ; BG 1 and 2 both use char tiles

   ; Load character set into VRAM
   lda #$80
   sta VMAIN   ; VRAM stride of 1 word
   ldx #VRAM_CHARSET
   stx VMADDL
   ldx #0
@charset_loop:
   lda NESfont,x
   stz VMDATAL ; color index low bit = 0
   sta VMDATAH ; color index high bit set -> neutral red (2)
   inx
   cpx #(128*8)
   bne @charset_loop

   ldx #0

   ; Note there is a bug here causing the color indexes to be offset
   ; This was fixed by just offsetting the palette
@spritetile_loop_low:
   lda spritemap,x
   sta VMDATAL
   inx
   inx
   lda spritemap,x
   sta VMDATAH
   ; inx
   ; inx
   cpx #(spritemap_end - spritemap) ; count * size * tile_size + stride-1
   bne @spritetile_loop_low
   ldx #1
;@spritetile_loop_high:
;   lda spritemap,x
;   sta VMDATAL
;   inx
;   inx
;   lda spritemap,x
;   sta VMDATAH
;   inx
;   inx
;   cpx #(2*4*8 + 1) ; count * size * tile_size + stride-1
;   bne @spritetile_loop_high

   ; Place string tiles in background
   ldx #START_TM_ADDR
   stx VMADDL
   ldx #0
@string_loop:
   lda hello_str,x
   beq @enable_display
   sta VMDATAL
   lda #$20 ; priority 1
   sta VMDATAH
   inx
   bra @string_loop

@enable_display:
   ; Show BG1 + Sprites
   lda #%00010001
   sta TM
   ; Maximum screen brightness
   lda #$0F
   sta INIDISP

   ; enable NMI and Auto Joypad read for Vertical Blank
   lda #%10000001 ; Enable NMI and Auto Joypad read
   sta NMITIMEN

game_loop:
   wai ; Pause until next interrupt complete (i.e. V-blank processing is done)
   ; jsr read_input
;   rep #$20
input_loop:
   lda $4212
   bit #$0001
   bne input_loop
   lda JOY1H
   sta Joy1A

   ; Update Aim

   bit #$04 ; KEY_DOWN
   beq Down_not_held
      lda aim_y
      inc
      inc
      sta aim_y
Down_not_held:

   lda Joy1A
   bit #$08 ; KEY_UP
   beq Up_not_held
      lda aim_y
      dec
      dec
      sta aim_y
Up_not_held:

   lda Joy1A
   bit #$02 ; KEY_LEFT
   beq Left_not_held
      lda aim_x
      dec
      dec
      sta aim_x
Left_not_held:

   lda Joy1A
   bit #$01 ; KEY_RIGHT
   beq Right_not_held
      lda aim_x
      inc
      inc
      ;clc
      ;adc #$02
      sta aim_x
Right_not_held:


   ldx aim_obj
   lda aim_x
   sta oam_lo_buffer, x
   lda aim_y
   inx
   sta oam_lo_buffer, x

   ; Bubble Update
   ldx #BubbleStart
BubbleUpdateLoop:
   inx
   ; Read Y
   lda oam_lo_buffer, x
   dec
   sta oam_lo_buffer, x
   inx
   inx
   inx
   cpx #(BubbleStart + (BubbleCount * 4))
   bne BubbleUpdateLoop
   

   jsr CpyToDMA
   jmp game_loop

nmi:
   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit
   phd
   pha
   phx
   phy
   ; Do stuff that needs to be done during V-Blank
   lda RDNMI ; reset NMI flag
   ply
   plx
   pla
   pld
return_int:
   rti

;----------------------------------------------------------------------------
; ClearVRAM -- Sets every byte of VRAM to zero
; from bazz's VRAM tutorial
; In: None
; Out: None
; Modifies: flags
;----------------------------------------------------------------------------
ClearVRAM:
   pha
   phx
   php

   REP #$30          ; mem/A = 8 bit, X/Y = 16 bit
   SEP #$20

   LDA #$80
   STA $2115         ;Set VRAM port to word access
   LDX #$1809
   STX $4300         ;Set DMA mode to fixed source, WORD to $2118/9
   LDX #$0000
   STX $2116         ;Set VRAM port address to $0000
   STX $0000         ;Set $00:0000 to $0000 (assumes scratchpad ram)
   STX $4302         ;Set source address to $xx:0000
   LDA #$00
   STA $4304         ;Set source bank to $00
   LDX #$FFFF
   STX $4305         ;Set transfer size to 64k-1 bytes
   LDA #$01
   STA $420B         ;Initiate transfer

   STZ $2119         ;clear the last byte of the VRAM

   plp
   plx
   pla
   RTS

ClearOAM:
   pha
   phx
   php
   lda #0
   ldx #0
ClearOAM_Loop:
   sta oam_lo_buffer, x
   inx
   cpx #(oam_buffer_end - oam_lo_buffer)
   bne ClearOAM_Loop

   ; rep #$20
   lda #$FF ; set obj.pos.x (most significant bit isn't part of obj.pos.x and the least significant 2 are omitted)
   ldx #$00
   ldy #$00
SetAllSpritesOutside_Loop:
   sta oam_lo_buffer, y
   iny
   iny
   iny
   iny ; jump to next obj
   cpy #$0200
   bne SetAllSpritesOutside_Loop


   ; Setup Tile
   ldx #$42
   stx oam_lo_buffer
   ; Set sprite 0 Y position
   ldx #$67
   stx oam_lo_buffer + 1
   ; Set sprite 0 to priority 3 and tile 0x01
   ldx #((%00110000 << 8) | $0040)
   stx oam_lo_buffer + 2

   ; Set sprite 0 to be large (16x16)
   lda #%00000010
   sta oam_hi_buffer

   ; Write X
   ldx #BubbleStart
   lda #$19
BubbleInit_loop_x:
   sta oam_lo_buffer, X
   inx
   inx
   inx
   inx
   clc
   adc #$19
   cpx #(BubbleStart + (BubbleCount * 4))
   bne BubbleInit_loop_x

   ; Write Y
   ldx #(BubbleStart + 1)
   lda #$18
BubbleInit_loop_y:
   sta oam_lo_buffer, X
   inx
   inx
   inx
   inx
   cpx #(BubbleStart + (BubbleCount * 4) + 1)
   bne BubbleInit_loop_y

   ldx #(BubbleStart + 2)
   REP #$20
   ldy #((%00110000 << 8) | $0011)
   tya
BubbleInit_loop_attr:
   sta oam_lo_buffer, X
   inx
   inx
   inx
   inx
   cpx #(BubbleStart + (BubbleCount * 4) + 2)
   bne BubbleInit_loop_attr

   plp
   plx
   pla
   rts


CpyToDMA:
   pha
   phx
   php

   ; Copy OAM data via DMA
   stz OAMADDL
   lda #%00000000
   sta $4310 ; DMAP1
   lda #<OAMDATA
   sta $4311 ; BBAD1
   ldx #.loword(oam_lo_buffer)
   stx $4312 ; A1T1L
   lda #^oam_lo_buffer
   sta $4314 ; A1B1
   ldx #(oam_buffer_end - oam_lo_buffer)
   stx $4315 ; DAS1L
   lda #%00000010
   sta MDMAEN

   plp
   plx
   pla
   rts

   

;read_input:
;   lda JOY1L
;   sta Joy1A
;   lda JOY1H
;   sta Joy1B
;   rts
;

.include "charset.asm"

.segment "VECTORS"
.word 0, 0        ;Native mode vectors
.word return_int  ;COP
.word return_int  ;BRK
.word return_int  ;ABORT
.word nmi         ;NMI
.word start       ;RST
.word return_int  ;IRQ

.word 0, 0        ;Emulation mode vectors
.word return_int  ;COP
.word 0
.word return_int  ;ABORT
.word nmi         ;NMI
.word start       ;RST
.word return_int  ;IRQ