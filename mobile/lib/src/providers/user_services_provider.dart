import 'package:mobile/src/models/user_service.dart';
import 'package:flutter/cupertino.dart';
import 'dart:io';

class UserServiceProvider extends ChangeNotifier {

  /// List of [Service] related to the user
  List<UserService> userServices = [];

  /// Adds a service into the Provider
  addServiceForUser(UserService newService) {
    userServices.add(newService);
    notifyListeners();
  }

  /// Modifies a service given as argument
  modifyService(UserService toModify, String serviceAccountId, String accountUsername, String accountSlug, String userExternalToken) {
    for (int i = 0; i < userServices.length; i++) {
      if (userServices[i].serviceName == toModify.serviceName &&
          userServices[i].serviceAccountId == toModify.serviceAccountId &&
          userServices[i].userExternalToken == toModify.userExternalToken) {
        UserService newService = UserService(
          serviceName: userServices[i].serviceName,
          serviceAccountId: serviceAccountId,
          accountUsername: accountUsername,
          accountSlug: accountSlug,
          userExternalToken: userExternalToken
        );
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
      if (uService.serviceName == toRemove.serviceName &&
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