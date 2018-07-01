(ns bucks.changes)


(defn- history
  ([version features breaking-changes] (history version nil features breaking-changes))
  ([version url features breaking-changes]
   (let [style {:list-style "disc" :margin-left "20px"}
         version-1 (if url version [version [:small " (current)"]])
         features (->> features
                       (map-indexed (fn [i f] [:li {:key i :style style} f]))
                       (into [[:li.has-text-info "Features"]]))
         breaking-changes (if (empty? breaking-changes)
                            []
                            (->> breaking-changes
                                 (map-indexed (fn [i f] [:li {:key (str "e" i)  :style style} f]))
                                 (into [[:li.has-text-danger "Breaking Changes"]])))]
     [:div
      [:h1.has-text-primary "V" version-1 (when url [:small [:a {:href url} " (view here)"]])]
      [:ul
       features
       breaking-changes]
      [:br]]
     )))


(defn render-history []
  [:div.has-text-light
   (history
    "1.x"
    ["Todo Unitization should look from 1 jan to 1 jan (not 1 jan to 31 dec)"
     "Todo Calculate RA Contributions"
     "Todo monthly transactions bar chart"
     "Todo Retirement goals chart"
     "Todo Fix year data"
     "Todo add dividends"
     "Add Asset Comparison Chart"
     "Add all time performance and growth to assets"
     "Add Asset Types for Property and Business"
     "Hide Confusing Asset/Asset Group %"]
    [])
   (history
    "1.24"
    "/bucks-v1.24"
    ["Unitize assets for better performance tracking"
     "Add actual independecy trend line"
     "Fix Bug where todays values do not show"
     "Add Cash Asset Type"
     "Remove yearly trend lines to reduce noise"
     "Improve WI Labels"
     "Hide closed asset groups"
     "Reverse years order"]
    [])
   (history
    "1.16"
    "/bucks-v1.16"
    ["Added Asset Type Distributions over time graphs"
     "Calculate Average Savings Rate"
     "Graph Expected and Actual Savings Rate over time"
     "Calculate Estimate Years To Retirement https://www.mrmoneymustache.com/2012/01/13/the-shockingly-simple-math-behind-early-retirement/"
     "Show Estimate Years To retirement for different savings rates"
     "Graph Years to Independence over time with goal line"
     "Improve Pie Chart Legend"
     "Improve Wealth Index Goal Chart"
     "Change Lookback window from 6 Months to 12 Months"]
    [])
   (history
    "1.7"
    "/bucks-v1.7"
    ["Track TFSA seperately from asset types. To view a TFSA asset in the TFSA section set 'is-tfsa' to 'y'"
     "Allow Multiple Salaries"
     "Added 'Actively Managed' asset type"
     "Allow 'comment' rows"]
    ["'open-asset' requires an 'is-tfsa' field to be added to track TFSA's seperately. Can be 'y' or 'n'. Add to the end of the row"
     "'salaries' requires an 'owner' field. This is the first field after 'salaries'"])
   (history
    "1.3"
    "/bucks-v1.3"
    ["Tax Free Savings Reporting"
     "Monthly Income and Expense Tracking. Use the 'income-expense' row."
     "Retirement Numbers based on the 4% rule. Requires monthly income and expense."
     "'Emergency Fund' asset type. Used to calculate 'MONTHS COVERED BY EMERGENCY FUND'"
     "Wealth Distribution by owne"]
    ["'open-asset' requires an 'owner' field to be added to track TFSA's"])
   (history
    "1"
    "/bucks-v1"
    ["Initial Release"]
    [])])
