import gleam/erlang/node

import gleeunit
import gleeunit/should

import mail_tracker

pub fn main() {
  //Install our tables - ignore all errors
  should.be_ok(mail_tracker.install([node.self()]))

  //Check the system
  //mail_tracker.system_info()

  gleeunit.main()
}

//
pub fn add_test() {
  //Try adding something
  let x =
    mail_tracker.add_item(mail_tracker.MailTracker(
      "0",
      "Someone",
      "8 Somewhere Lane, Sometown USA",
      "Another",
      "18 Hill Avenue, London England",
    ))
  should.be_ok(x)
}

pub fn get_item_by_to_test() {
  //Try getting something
  let x = mail_tracker.get_item_by_to("Someone")
  should.be_ok(x)
}

pub fn get_item_by_from_test() {
  //Try getting something
  let x = mail_tracker.get_item_by_from("Another")
  should.be_ok(x)
}

pub fn delete_item_by_id_test() {
  let x = mail_tracker.delete_item_by_id("0")
  should.be_ok(x)
}

pub fn fail_get_item_by_id_test() {
  //Try getting something
  let x = mail_tracker.get_item_by_id("0")
  should.be_error(x)
}

pub fn insert_multiple_records_test() {
  //Insert two records with the same recipient
  should.be_ok(
    mail_tracker.add_item(mail_tracker.MailTracker(
      "0",
      "Someone",
      "8 Somewhere Lane, Sometown USA",
      "Another",
      "18 Hill Avenue, London England",
    )),
  )
  should.be_ok(
    mail_tracker.add_item(mail_tracker.MailTracker(
      "1",
      "Someone",
      "8 Somewhere Lane, Sometown USA",
      "Other",
      "18 Lawn Road, Helsinki Finland",
    )),
  )
}

pub fn list_db_test() {
  let x = mail_tracker.list_db()
  //io.debug(x)
  should.be_ok(x)
}

pub fn retrieve_multiple_records_test() {
  //Try getting something
  let x = mail_tracker.get_item_by_to("Someone")
  should.be_ok(x)
}
