import 'package:flutter/material.dart';
import 'package:mobile/src/models/service.dart';

// Object representation of a pipeline trigger
class Trigger {
  // Service triggering
  final Service service;
  // Triggering Action name;
  // TODO Might be an enum
  final String action;
  /// Last time the triggered was done
  final DateTime last;
  const Trigger(
      {Key? key,
      required this.service,
      required this.action, required this.last});
  // TODO Constructor from DB 'Type' field
}
