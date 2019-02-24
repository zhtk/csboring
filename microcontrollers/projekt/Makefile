CC       = arm-eabi-gcc
OBJCOPY  = arm-eabi-objcopy
FLAGS    = -mthumb -mcpu=cortex-m4
CPPFLAGS = -DSTM32F411xE
CFLAGS   = $(FLAGS) -Wall -g \
	-O2 -ffunction-sections -fdata-sections \
	-I/opt/arm/stm32/inc \
	-I/opt/arm/stm32/CMSIS/Include \
	-I/opt/arm/stm32/CMSIS/Device/ST/STM32F4xx/Include
LDFLAGS  = $(FLAGS) -Wl,--gc-sections -nostartfiles \
	-specs=nosys.specs \
	-L/opt/arm/stm32/lds -Tstm32f411re.lds
vpath %.c /opt/arm/stm32/src

OBJECTS = main.o usart.o i2c.o sensors.o startup_stm32.o delay.o gpio.o lcd.o fonts.o
TARGET  = projekt

.SECONDARY: $(TARGET).elf $(OBJECTS)
all: $(TARGET).bin
%.elf : $(OBJECTS)
	$(CC) $(LDFLAGS) $^ -o $@
%.bin : %.elf
	$(OBJCOPY) $< $@ -O binary
flash: all
	openocd -d0 \
		-f board/st_nucleo_f4.cfg \
		-c "init" -c "reset halt" \
		-c "flash write_image erase projekt.bin 0x8000000" \
		-c "verify_image projekt.bin" -c "reset" -c "shutdown"
clean :
	rm -f *.bin *.elf *.hex *.d *.o *.bak *~
