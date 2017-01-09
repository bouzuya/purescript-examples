var fetch = require('isomorphic-fetch');
var fetchImpl = typeof global === 'undefined'
  ? fetch : global.fetch.bind(global);

exports.fetchImpl = function (request) {
  var url = request.url;
  var init = Object.assign({}, request);
  delete init.url;
  return function (success) {
    return function (failure) {
      return function () {
        return void fetchImpl(url, init)
          .then(function (response) {
            var status = response.status;
            if (status < 200 || 300 < response) {
              return Promise.reject(new Error('status : ' + status));
            } else {
              return response.text();
            }
          })
          .then(function (response) {
            success(response)();
          }, function (error) {
            failure(error)();
          });
      };
    };
  };
};
