{
 "cells": [
  {
   "cell_type": "markdown",
   "id": "568b0200",
   "metadata": {
    "papermill": {
     "duration": 0.001875,
     "end_time": "2025-01-04T06:28:12.734128",
     "exception": false,
     "start_time": "2025-01-04T06:28:12.732253",
     "status": "completed"
    },
    "tags": []
   },
   "source": []
  },
  {
   "cell_type": "markdown",
   "id": "426efdac",
   "metadata": {
    "papermill": {
     "duration": 0.001173,
     "end_time": "2025-01-04T06:28:12.736821",
     "exception": false,
     "start_time": "2025-01-04T06:28:12.735648",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# **Motivation**\n",
    "\n",
    "We were fortunate to have an insightful conversation with quarterback Josh Rosen, forming the inspiration for our project. Weaving anecdotes from his time in the NFL with X’s and O’s, a major point of emphasis was along the lines of:\n",
    "\n",
    "If I could know with 100% certainty what the safeties would do, my life would be twice as easy.\n",
    "\n",
    "Why? For example, suppose the offense runs a [\"Hoss\"](https://fishduck.com/2023/11/the-oregon-offense-the-hoss-concept/) concept (hitch-seam combo) on both sides. If the offense knows the defense is playing [\"Middle-Of-Field-Closed coverage\"](https://www.si.com/nfl/colts/film/gus-glossary-mofo-mofc) (MOFC) – one safety in the middle of the field – there is a strong chance one of the vertical routes will be open, because the safety can’t cover both. If the offense knows the defense is playing Middle-Of-Field-Open (MOFO), the QB’s first read is likely one of the hitches, since safeties can cover both slot routes. Having that pre-snap knowledge gives the offense a real advantage.\n",
    "\n",
    "Hence, in this study, we evaluate the play-level and team-level predictability of safeties. We devise a novel and powerful model which, given safeties’ pre-snap positioning and movements, estimates their post-snap middle-of-field coverage. We then quantify a defense’s overall level of safety predictability via safety entropy. Understanding opposing safeties’ degree of unpredictability allows an offense to tailor its game preparations. From our model, we extract key features – concrete defensive signals that telegraph what the safeties will do. In conjunction with play-level safety entropy, these tells can help safeties and quarterbacks retroactively study safety movements. Safeties can learn to disguise more effectively, whereas quarterbacks can work on diagnosing safeties’ signals.\n"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "528c045e",
   "metadata": {
    "papermill": {
     "duration": 0.001148,
     "end_time": "2025-01-04T06:28:12.739146",
     "exception": false,
     "start_time": "2025-01-04T06:28:12.737998",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# **Data**\n",
    "\n",
    "We consider a subset of plays from Weeks 1-9, 2022, focusing on longer early-down dropbacks (1st & ≥10 or 2nd & ≥5). We exclude garbage time (win_probability (0.05, 0.95)), late-half plays (half_seconds_remaining  30), and obvious pass situations (xpass  0.95, using [\"expected dropback probability\"](https://opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/#expected-dropback-model-features) from nflFastR).\n",
    "\n",
    "Our binary outcome variable, MOFO, is 1 if the safeties leave the middle of the field open, else 0. We derive MOFO from pff_passCoverage, which we estimate as 90-95% accurate from film study: MOFO is 1 for 2-Man or variations of Cover 0, 2, 4, or 6 (zero or two deep safeties post-snap) and 0 for variations of Cover 1 or 3 (one deep safety). We exclude dropbacks with miscellaneous coverages (e.g., Prevent, Goal Line).\n",
    "\n",
    "We classify a defender as a pre-snap safety if he is ≥8.5 yards from the line of scrimmage at some time between the offensive line being set and the snap, and if he is not the right-most or left-most defender at the snap. Filtering for plays with one or two pre-snap safeties yields 4,381 plays (1,352 single-safety, 3,029 two-safety). Just 113 of 7,410 safety-play combinations (1.5%) deviate from official NFL position labels, validating our definition."
   ]
  }
 ],
 "metadata": {
  "kaggle": {
   "accelerator": "none",
   "dataSources": [],
   "isGpuEnabled": false,
   "isInternetEnabled": true,
   "language": "r",
   "sourceType": "notebook"
  },
  "kernelspec": {
   "display_name": "R",
   "language": "R",
   "name": "ir"
  },
  "language_info": {
   "codemirror_mode": "r",
   "file_extension": ".r",
   "mimetype": "text/x-r-source",
   "name": "R",
   "pygments_lexer": "r",
   "version": "4.4.0"
  },
  "papermill": {
   "default_parameters": {},
   "duration": 3.417743,
   "end_time": "2025-01-04T06:28:12.862073",
   "environment_variables": {},
   "exception": null,
   "input_path": "__notebook__.ipynb",
   "output_path": "__notebook__.ipynb",
   "parameters": {},
   "start_time": "2025-01-04T06:28:09.444330",
   "version": "2.6.0"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
