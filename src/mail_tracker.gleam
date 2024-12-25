import gleam/erlang/node
import gleam/io

pub type MailTracker {
  MailTracker(
    id: String,
    to: String,
    to_address: String,
    from: String,
    from_address: String,
  )
}

pub fn main() {
  //Greeter
  io.println("Hello from mail_tracker!")
}

// Convert a tuple returned from Mnesia into a MailTracker type
pub fn tuple_to_mail_tracker(
  input: #(String, String, String, String, String),
) -> MailTracker {
  MailTracker(input.0, input.1, input.2, input.3, input.4)
}

// Get the current state of the Mnesia DB
@external(erlang, "mail_tracker_ffi", "system_info")
pub fn system_info() -> String

// Install the tables into a list of nodes
@external(erlang, "mail_tracker_ffi", "install")
pub fn install(input: List(node.Node)) -> Result(Nil, Nil)

// Public part of add_item - takes a MailTracker type, and then calls the internal add_item function after splitting up the type
pub fn add_item(input: MailTracker) -> Result(Nil, String) {
  add_item_internal(
    input.id,
    input.to,
    input.to_address,
    input.from,
    input.from_address,
  )
}

// Internal function to add an item to the Mnesia DB
@external(erlang, "mail_tracker_ffi", "add_item")
fn add_item_internal(
  id: String,
  to: String,
  to_address: String,
  from: String,
  from_address: String,
) -> Result(Nil, String)

// Retrieve an item by its id
@external(erlang, "mail_tracker_ffi", "get_item_by_id")
pub fn get_item_by_id(
  to: String,
) -> Result(#(String, String, String, String, String), String)

// Retrieve an item by its recipient name
@external(erlang, "mail_tracker_ffi", "get_item_by_to")
pub fn get_item_by_to(
  to: String,
) -> Result(List(#(String, String, String, String, String)), String)

// Retrieve an item by its sender name
@external(erlang, "mail_tracker_ffi", "get_item_by_from")
pub fn get_item_by_from(
  to: String,
) -> Result(#(String, String, String, String, String), String)

// Delete an item by its recipient name
@external(erlang, "mail_tracker_ffi", "delete_item_by_to")
pub fn delete_item_by_to(input: String) -> Result(Nil, String)

// Delete an item by its id
@external(erlang, "mail_tracker_ffi", "delete_item_by_id")
pub fn delete_item_by_id(input: String) -> Result(Nil, String)

// Print out a datbase's keys
@external(erlang, "mail_tracker_ffi", "list_db")
pub fn list_db() -> Result(String, String)
