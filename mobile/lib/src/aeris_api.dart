import 'dart:async';
import 'package:aeris/src/models/action.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/models/trigger.dart';

/// Call to interact with Aeris' Back end
class AerisAPI {
  ///TODO set status based on stored credentials
  bool connected = true;
  late List<Pipeline> fakeAPI;

  AerisAPI() {
    var trigger1 = Trigger(
        service: const Service.spotify(),
        name: "Play song",
        last: DateTime.now());
    var trigger3 = Trigger(
        service: const Service.discord(),
        name: "Send a message",
        last: DateTime.now());
    var trigger2 = Trigger(
        service: const Service.spotify(),
        name: "Play song",
        last: DateTime.parse("2022-01-01"));
    var reaction = Reaction(
        service: const Service.twitter(), parameters: {}, name: "Post a tweet");
    var reaction2 = Reaction(
        service: const Service.gmail(), parameters: {}, name: "Do smth");
    var reaction1 = Reaction(
        service: const Service.youtube(), parameters: {}, name: "Do smth youtube");
    var pipeline1 = Pipeline(
        id: 10,
        name: "My Action",
        triggerCount: 1,
        enabled: true,
        trigger: trigger1,
        reactions: [reaction]);
    var pipeline2 = Pipeline(
        id: 10,
        name: "My very long action Action",
        triggerCount: 10,
        enabled: true,
        trigger: trigger2,
        reactions: [reaction, reaction1, reaction2]);
    var pipeline3 = Pipeline(
        id: 10,
        name: "Disabled",
        triggerCount: 3,
        enabled: false,
        trigger: trigger3,
        reactions: [reaction]);
    fakeAPI = [
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1
    ];
  }

  /// Adds new pipeline to API
  Future<void> createPipeline(Pipeline newPipeline) async {
    ///TODO Send Pipeline to API
    fakeAPI.add(newPipeline);
    await Future.delayed(const Duration(seconds: 4));
    return;
  }

  /// Removes pipeline from API
  Future<void> removePipeline(Pipeline pipeline) async {
    ///TODO Send delete request to API
    fakeAPI.remove(pipeline);
    await Future.delayed(const Duration(seconds: 4));
    return;
  }

  Future<void> editPipeline(Pipeline updatedPipeline) async {
    ///TODO Send update request to API
    for (var pipeline in fakeAPI) {
      if (pipeline.id == updatedPipeline.id) {
        ///TODO Call Api
        break;
      }
    }

    await Future.delayed(const Duration(seconds: 4));
    return;
  }

  /// Fetches the Pipelines from the API
  Future<List<Pipeline>> getPipelines() async {
    /// TODO Fetch the API
    await Future.delayed(const Duration(seconds: 5));
    return fakeAPI;
  }

  /// Disconnects the user from the service
  Future<void> disconnectService(Service service) async {
    ///TODO disconnect service from user
    await Future.delayed(const Duration(seconds: 4));
    return;
  }

  Future<List<ActionTemplate>> getActionsFor(
      Service service, Action action) async {
    await Future.delayed(const Duration(seconds: 3));
    if (action is Trigger) {
      ///TODO get triggers
    } else if (action is Reaction) {
      ///TODO get reactions
    }
    return [
      for (int i = 0; i <= 10; i++)
        ActionTemplate(
            service: service,
            name: "action$i",
            parameters: {'key1': 'value1', 'key2': 'value2'})
    ];
  }
}
