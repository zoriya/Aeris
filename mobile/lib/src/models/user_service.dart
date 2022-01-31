// Class for a service related to a user (Youtube, Gmail, ...)
import 'package:mobile/src/models/service.dart';
import 'package:flutter/cupertino.dart';

class UserService {

  /// Service name related to the user
  final String serviceName;

  /// Id of an user for this service
  final String serviceAccountId;

  /// Account Username
  final String accountUsername;

  /// Account Slug for this Service
  final String accountSlug;

  /// Account External Token for this Service
  final String userExternalToken;

  UserService({Key? key, required this.serviceAccountId, required this.accountUsername, required this.accountSlug, required this.userExternalToken, required this.serviceName});

}
