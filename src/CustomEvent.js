exports.customEvent = (name) => (detail) => {
  return new CustomEvent(name, {
    detail
  })
}
