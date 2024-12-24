import gleam/erlang/node
import gleam/io

pub type MailTracker {
  MailTracker(
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

@external(erlang, "mail_tracker_ffi", "system_info")
pub fn system_info() -> String

@external(erlang, "mail_tracker_ffi", "install")
pub fn install(input: List(node.Node)) -> Result(Nil, Nil)

@external(erlang, "mail_tracker_ffi", "add_item")
pub fn add_item(
  to: String,
  to_address: String,
  from: String,
  from_address: String,
) -> Result(Nil, String)

@external(erlang, "mail_tracker_ffi", "get_item_by_to")
pub fn get_item_by_to(
  to: String,
) -> Result(#(String, String, String, String), String)
