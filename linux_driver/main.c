#include "monterko.h"

MODULE_AUTHOR("Piotr Zalas");
MODULE_LICENSE("GPL");

DEFINE_MUTEX(globallock);

static struct pci_device_id
monter_device_id[2] = {
	{
		.vendor = MONTER_VENDOR_ID,
		.device = MONTER_DEVICE_ID,
		.subvendor = PCI_ANY_ID,
		.subdevice = PCI_ANY_ID,
	}, 
	{}
};

static struct pci_driver 
monter_driver = {
	.name = DEVICE_NAME,
	.id_table = monter_device_id,
	.probe    = monter_probe,
	.remove   = monter_remove,
};

struct class *monter_class;
struct kmem_cache *context_alloc;
DEFINE_SPINLOCK(spinlock);
unsigned long spinflags;

struct file_operations monter_operations = {
	.owner = THIS_MODULE,
	
	.write = monter_write,
	.unlocked_ioctl = monter_ioctl,
	.compat_ioctl = monter_ioctl,
	.mmap = monter_mmap,
	.open = monter_open,
	.release = monter_release,
	.fsync = monter_fsync,
};

static int 
monter_init_module(void)
{
	printk(KERN_WARNING "monter: loading module\n");
	monter_class = class_create(THIS_MODULE, DEVICE_NAME);
	
	if (monter_class == NULL)
		goto init_error;
	
	context_alloc = KMEM_CACHE(monter_context, 0);
	
	if (context_alloc == NULL)
		goto init_class_error;
	
	if (pci_register_driver(&monter_driver))
		goto init_alloc_error;

	printk(KERN_WARNING "monter: module ready\n");
	return 0;
	
	init_alloc_error:
	kmem_cache_destroy(context_alloc);
	
	init_class_error:
	class_destroy(monter_class);
	
	init_error:
	return -1;
}

static void
monter_cleanup_module(void)
{
	pci_unregister_driver(&monter_driver);
	kmem_cache_destroy(context_alloc);
	class_destroy(monter_class);
	
	printk(KERN_WARNING "monter: module unloaded\n");
	return;
}

module_init(monter_init_module);
module_exit(monter_cleanup_module);
