const { main } = require('./output/Main')
const { FocusMenu } = require('./src/FocusMenu')

window.customElements.define('focus-menu', FocusMenu)

const app = document.createElement('div')
app.setAttribute("id", "app")
document.body.appendChild(app)

main()
