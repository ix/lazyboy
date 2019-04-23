;; DEFINE THE ROM START LOCATION
SECTION "rom", ROM0

;; DEFINE THE START LOCATION AS 0x100
SECTION "start", ROM0[$0100]
    nop
    jp main

;; PLACE THE MAIN BLOCK AT 0x150
SECTION "main", ROM0[$0150]

;; DEFINE SOME CONSTANTS
BGP EQU $FF47
LY EQU $FF44
LCDC EQU $FF40
BG EQU $9800

main:
    ld hl, $C00C
ld [hl],$ef
ld hl, $C00D
ld [hl],$ab
ld hl, $C00E
ld [hl],$ab

    call wait_vblank

wait_vblank:
    ldh a, [LY]
    cp $91
    jr nz, wait_vblank
    ret
