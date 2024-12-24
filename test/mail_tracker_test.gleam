import gleam/erlang/node
import gleam/io
import gleeunit
import gleeunit/should
import mail_tracker

pub fn main() {
  //Install our tables - ignore all errors
  case mail_tracker.install([node.self()]) {
    _ -> ""
  }

  //Check the system
  mail_tracker.system_info()

  gleeunit.main()
}

//
pub fn add_test() {
  //Try adding something
  should.be_ok(mail_tracker.add_item(
    "Someone",
    "8 Somewhere Lane, Sometown USA",
    "Another",
    "18 Hill Avenue, London England",
  ))
}

pub fn get_item_by_to_test() {
  //Try getting something
  let x = mail_tracker.get_item_by_to("Someone")
  io.debug(x)
  should.be_ok(x)
}
