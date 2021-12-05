/* elm-pkg-js
port localstorage_store : String -> Cmd msg
port localstorage_load : { } -> Cmd msg
port localstorage_loaded : (String -> msg) -> Sub msg
*/

exports.init = async function (app) {
    app.ports.localstorage_store.subscribe(function (text) {
        try {
            localStorage.setItem("model", text);
        } catch (e) {
        }
    });

    app.ports.localstorage_load.subscribe(function (_) {
        try {
            let model = localStorage.getItem("model");
            app.ports.localstorage_loaded.send(model);
        } catch (e) {
            app.ports.localstorage_loaded.send("");
        }
    });
}
