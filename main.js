const { main } = require('./output/Main')

const app = document.createElement('div')
app.setAttribute("id", "app")
document.body.appendChild(app)

main()
