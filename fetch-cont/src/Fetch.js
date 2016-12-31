var fetch = require('isomorphic-fetch');
var fetchImpl = typeof global === 'undefined'
  ? fetch : global.fetch.bind(global);

exports.fetchImpl = function (request) {
  return function (success) {
    return function (failure) {
      return function () {
        return void fetchImpl(request)
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
