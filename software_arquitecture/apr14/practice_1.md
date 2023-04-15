# Inventory System

### Requirements
1) The system must allow real-time follow-ups from given products in inventory
2) The system must allow management of new product orders and update the inventory as soon as a product is sold
3) The system must be easy-to-use and have an intuitive user interface such that store employees must be able to use it without any formal specialty
4) The system must be scalable, which means that that it must be able to handle a great deal of data and a large number of users
5) The system must be safe and protect inventory data from external or internal liabilities
6) the system must be compatible with the existing systems used by the store, such as the sales point system or the accountability system
7) The system must be capable of gerenating detailed reports about inventory, such as stock levels, inventory movements and most sold products
8) The system must be easily maintainable and a good support to ensure that it is always updated and working

### System components
- Auth module (to control connections and control data)
- Log system (security, logins, issues, etc)
- Inventory Module (database, orm, mpi)
- Integration handler (to allow data IO with other systems)
- Report Module (to generate reports about inventory, stock and movements)
- User interface (with a good user experience)

### Connections
Auth module -> Log system | 
User interface -> Auth module | Inventory Module | Report Module | Integration Handler |
Inventory Module -> Auth Module | Report Module | Integration handler |
Integration Handler -> Auth Module | Inventory Module | Log System |
Log system -> Auth Module
Report Module -> Auth Module | Inventory Module |

### Architecture style
- Event driven

### Architecture structure
- Modular decomposition/Microservices: Allows easy mantainment of independent modules, allows easy orquestration of responsabilities, depending on the event handler (say we choose kafka), allows for real-time responses of modules with the user and finally the auth modules makes sure that each request is made from an authentication module.
