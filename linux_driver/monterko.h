#ifndef MONTERKO_H
#define MONTERKO_H

#include <linux/kernel.h>
#include <linux/module.h> 
#include <linux/pci.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/list.h>
#include <linux/slab.h>
#include <linux/interrupt.h>
#include <linux/mm.h>
#include <asm/uaccess.h>

#include "monter.h"
#include "monter_ioctl.h"

#define DEVICE_NAME "monter"
// Maksymalna ilość urządzeń w systemie
#define MAX_DEVICES 256
// Limit na ilość asynchronicznych instrukcji
#define INSTRUCTION_LIMIT 32

struct monter_context {
	struct list_head users;
	struct monter_device_data *device;
	// Kolejka na fsync
	struct completion fsync;
	int fsync_counter;
	
	// Kolejka do zapisu
	int write_wait;
	struct mutex write_mutex;
	struct completion buffer_wait;
	
	// Bufor poleceń i liczba zapisanych slotów
	unsigned int inst[INSTRUCTION_LIMIT];
	int used;
	
	// Obszar roboczy
	dma_addr_t work_handle;
	void *work;
	unsigned long work_size;
	
	// dane rejestrów
	int addr_set;
	int new_reg_a;
	int new_reg_b;
	
	int reg_a;
	int reg_b;
};

struct monter_device_data {
	dev_t devt;
	struct cdev cdev;
	struct device *device;
	void __iomem *bar;
	
	// Struktury na główną kolejkę poleceń
	dma_addr_t inst_handle;
	void *instructions;
	
	// Lista kontekstów przypisanych do urządzenia
	struct monter_context *user;
	int active;
};

extern struct file_operations monter_operations;
extern struct class *monter_class;
extern struct mutex globallock;
extern struct kmem_cache *context_alloc;
extern spinlock_t spinlock;
extern unsigned long spinflags;

int monter_assign_id(void);
void monter_free_id(int id);
struct monter_device_data * monter_find_devt(dev_t dev);
void monter_run_context(struct monter_context *ct);
int monter_select_context(struct monter_context *ct);

int monter_probe(struct pci_dev *dev, const struct pci_device_id *id);
void monter_remove(struct pci_dev *dev);
irqreturn_t monter_irq(int irq, void *dev);
int monter_open(struct inode *, struct file *);
int monter_release(struct inode *, struct file *);
long monter_ioctl(struct file *fp, unsigned int, unsigned long);
ssize_t monter_write(struct file *, const char __user *, size_t, loff_t *);
int monter_mmap(struct file *fp, struct vm_area_struct *vm);
int monter_fsync(struct file *, loff_t, loff_t, int datasync);

#endif
