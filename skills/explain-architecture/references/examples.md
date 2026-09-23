# Diagram examples

Glossary assumed: **Order**, **Customer**, **Invoice**, **Shipment**; Ordering and Billing are contexts. Labels use those terms; node ids stay short.

## Modules and dependencies

```mermaid
flowchart LR
    Api["HTTP routes"] --> Ord["Order intake"]
    Api --> Bill["Invoicing"]
    Ord --> Store[("Order store")]
    Ord -- "OrderPlaced" --> Ful["Fulfillment"]
    Ful -- "ShipmentDispatched" --> Bill
    Bill --> Pay["Payment gateway adapter"]
```

## Main flow

```mermaid
sequenceDiagram
    participant C as Customer
    participant Api as HTTP routes
    participant Ord as Order intake
    participant Ful as Fulfillment
    C->>Api: place Order
    Api->>Ord: validate and save Order
    Ord-->>Api: Order id
    Ord-)Ful: OrderPlaced
    Ful-)Ful: pick and pack Shipment
```

## Core data

```mermaid
erDiagram
    CUSTOMER ||--o{ ORDER : places
    ORDER ||--|{ ORDER_LINE : contains
    ORDER ||--o| SHIPMENT : "ships as"
    SHIPMENT ||--|| INVOICE : triggers
```

Use classDiagram instead when behaviour on the types matters more than cardinality:

```mermaid
classDiagram
    class Order {
      +place()
      +cancel()
    }
    class Invoice {
      +issue()
    }
    Order --> Invoice : billed by
```
