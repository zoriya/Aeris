import 'package:aeris/src/models/user_service.dart';
import 'package:aeris/src/models/service.dart';
import 'package:flutter/cupertino.dart';

/// Provider used to store every Service related to the User
class UserServiceProvider extends ChangeNotifier {
  /// List of [Service] related to the user
  List<UserService> userServices = [];

  /// Adds a service into the Provider
  addServiceForUser(UserService newService) {
    userServices.add(newService);
    notifyListeners();
  }

  /// Creates a new service related to the user
  createUserService(Service serviceToSet,
      {String accountId = "",
      String accUsername = "",
      String accountSlug = "",
      String externalToken = ""}) {
    UserService newService = UserService(
        serviceAccountId: accountId,
        accountUsername: accUsername,
        accountSlug: accountSlug,
        userExternalToken: externalToken,
        serviceProvider: serviceToSet);
    userServices.add(newService);
    // notifyListeners(); /// TODO Get the notifyListeners method back.
  }

  /// Sets a list of service into the Provider
  setServiceForUser(List<UserService> newServices) {
    userServices = [];
    userServices = newServices;
    notifyListeners();
  }

  /// Modifies a service given as argument
  modifyService(UserService toModify, String serviceAccountId,
      String accountUsername, String accountSlug, String userExternalToken) {
    for (int i = 0; i < userServices.length; i++) {
      if (userServices[i].serviceProvider.name ==
              toModify.serviceProvider.name &&
          userServices[i].serviceAccountId == toModify.serviceAccountId &&
          userServices[i].userExternalToken == toModify.userExternalToken) {
        UserService newService = UserService(
            serviceProvider: userServices[i].serviceProvider,
            serviceAccountId: serviceAccountId,
            accountUsername: accountUsername,
            accountSlug: accountSlug,
            userExternalToken: userExternalToken);
        userServices[i] = newService;
        notifyListeners();
        return true;
      }
    }
    return false;
  }

  /// Removes a service from the Provider
  removeService(UserService toRemove) {
    for (UserService uService in userServices) {
      if (uService.serviceProvider.name == toRemove.serviceProvider.name &&
          uService.serviceAccountId == toRemove.serviceAccountId &&
          uService.userExternalToken == toRemove.userExternalToken) {
        userServices.remove(uService);
        notifyListeners();
        return true;
      }
    }
    return false;
  }

  /// Clears Provider from data
  clearProvider() {
    userServices.clear();
    notifyListeners();
  }
}
