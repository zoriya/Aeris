import 'package:mobile/src/models/pipeline.dart';
import 'package:flutter/cupertino.dart';
import 'dart:io';

/// Provider class for Pipelines
class PipelineProvider extends ChangeNotifier {

  /// List of Pipelines stored in Provider
  List<Pipeline> pipelines = [];

  /// Adds a pipeline in the Provider
  addPipelineInProvider(Pipeline newPipeline) {
    pipelines.add(newPipeline);
    _sortsPipelines();
    notifyListeners();
  }

  /// Sets a new list of pipelines into the Provider
  setPipelineProvider(List<Pipeline> newPipelines) {
    pipelines = [];
    pipelines = newPipelines;
    _sortsPipelines();
  }

  _sortsPipelines() {
    pipelines.sort((a, b) {
      if (a.enabled == b.enabled) {
        return b.trigger.last.compareTo(a.trigger.last);
      }
      return b.enabled ? 1 : -1;
    });
  }

  /// Removes a specific pipeline from the Provider
  removePipelineFromProvider(int pipelineId) {
    for (Pipeline ppl in pipelines) {
      if (ppl.id == pipelineId) {
        // TODO: Remove the pipeline from the database or consult to do it out of the provider
        pipelines.remove(ppl);
        notifyListeners();
        return true;
      }
    }
    return false;
  }

  /// Removes every pipeline from the Provider
  clearProvider() {
    pipelines.clear();
    notifyListeners();
  }

}