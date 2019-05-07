;; DEFINE THE ROM START LOCATION
SECTION "rom", ROM0

;; DEFINE THE START LOCATION AS 0x100
SECTION "start", ROM0[$0100]
    nop
    jp main

;; PLACE THE MAIN BLOCK AT 0x150
SECTION "main", ROM0[$0150]

main:
jp L2
L1:
db $0,$0,$0,$0,$24,$24,$0,$0,$81,$81,$7E,$7E,$0,$0,$0,$0
L2:
ld HL, L1
ld DE, $9010
ld B, 16
.L3:
ld A, [HL+]
ld [DE], A
inc DE
dec B
jp nz, .L3
ld HL, $9800
ld [HL], 1
ld HL, $FF47
ld [HL], 228
ld HL, $FF40
ld [HL], 129
.L4:
jp .L4

