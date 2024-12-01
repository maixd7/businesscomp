import streamlit as st
import numpy as np
import pandas as pd
import joblib
import plotly.graph_objects as go
import shap

naics_industries = [
    "Agriculture, Forestry, Fishing and Hunting",
    "Mining, Quarrying, and Oil and Gas Extraction",
    "Utilities",
    "Construction",
    "Manufacturing",
    "Wholesale Trade",
    "Retail Trade",
    "Transportation and Warehousing",
    "Information",
    "Finance and Insurance",
    "Real Estate and Rental and Leasing",
    "Professional, Scientific, and Technical Services",
    "Management of Companies and Enterprises",
    "Administrative and Support Services",
    "Educational Services",
    "Health Care and Social Assistance",
    "Arts, Entertainment, and Recreation",
    "Accommodation and Food Services",
    "Other Services",
    "Public Administration"
]
state_abbreviations = [
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", 
    "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
    "VA", "WA", "WV", "WI", "WY"
]
state_abbreviation_to_name = {
    "AL": "Alabama",
    "AK": "Alaska",
    "AZ": "Arizona",
    "AR": "Arkansas",
    "CA": "California",
    "CO": "Colorado",
    "CT": "Connecticut",
    "DE": "Delaware",
    "FL": "Florida",
    "GA": "Georgia",
    "HI": "Hawaii",
    "ID": "Idaho",
    "IL": "Illinois",
    "IN": "Indiana",
    "IA": "Iowa",
    "KS": "Kansas",
    "KY": "Kentucky",
    "LA": "Louisiana",
    "ME": "Maine",
    "MD": "Maryland",
    "MA": "Massachusetts",
    "MI": "Michigan",
    "MN": "Minnesota",
    "MS": "Mississippi",
    "MO": "Missouri",
    "MT": "Montana",
    "NE": "Nebraska",
    "NV": "Nevada",
    "NH": "New Hampshire",
    "NJ": "New Jersey",
    "NM": "New Mexico",
    "NY": "New York",
    "NC": "North Carolina",
    "ND": "North Dakota",
    "OH": "Ohio",
    "OK": "Oklahoma",
    "OR": "Oregon",
    "PA": "Pennsylvania",
    "RI": "Rhode Island",
    "SC": "South Carolina",
    "SD": "South Dakota",
    "TN": "Tennessee",
    "TX": "Texas",
    "UT": "Utah",
    "VT": "Vermont",
    "VA": "Virginia",
    "WA": "Washington",
    "WV": "West Virginia",
    "WI": "Wisconsin",
    "WY": "Wyoming",
    "DC": "District of Columbia",
}
stateData = pd.read_csv('state.csv')
industryData = pd.read_csv('industry.csv')

if "result" not in st.session_state:
    st.session_state.result = None
    st.session_state.probability = None

model = joblib.load('model.joblib')

st.title('Should this loan be approved?')
# "State","Industry","FranchiseCode","UrbanRural","Term","NoEmp","RealEstate","DisbursementGross","LowDoc","Proportion","MIS_Status"
state = st.selectbox("State", state_abbreviations) 
industry = st.selectbox("What indsutry is the business in", naics_industries)
franchiseCode  = st.radio("Is your business a franchise",['Yes','No'])
urbanRural = st.radio("What is your business location", ['Urban','Rural'])
term = st.number_input("Loan term length (in months)", step = 1, min_value=0, format = "%d")
noEmp = st.number_input("How many employees does your business have", step = 1, min_value=0, format = "%d")
disbursementGross = st.number_input("How much is the loan", step = 100, min_value=0, format = "%d")
sbaGross = st.number_input("How much does the SBA cover", step = 100, min_value=0, format = "%d")
lowDoc = st.radio("Is the loan in the low doc program",['Yes','No'])

if term > 20:
    realEstate = 1
else :
    realEstate = 0
if disbursementGross > 0:
    proportion = sbaGross/disbursementGross

def predict(): 
    row = np.array([state,industry,franchiseCode,urbanRural,term,noEmp,realEstate,disbursementGross,lowDoc,proportion]) 
    columns = ['State', 'Industry', 'FranchiseCode', 'UrbanRural', 'Term', 'NoEmp', 
            'RealEstate', 'DisbursementGross', 
            'LowDoc', 'Proportion']
    X = pd.DataFrame([row], columns = columns)
    X['FranchiseCode'] = X['FranchiseCode'].map({'Yes': 1, 'No': 0})
    X['UrbanRural'] = X['UrbanRural'].map({'Urban': 1, 'Rural': 0})
    X['LowDoc'] = X['LowDoc'].map({'Yes': 1, 'No': 0})
    prediction = model.predict(X)
    probability = model.predict_proba(X)[0][1]
    if prediction[0] == 0: 
        st.session_state.result = 'Approve :thumbsup:'
    else: 
        st.session_state.result = 'Reject :thumbsdown:' 
    st.session_state.probability = probability * 100
    st.session_state.X = X

st.button('Predict', on_click=predict)

if st.session_state.result:
    st.write(st.session_state.result)

    # Create the risk meter
    fig = go.Figure(go.Indicator(
        mode="gauge+number",
        value=st.session_state.probability,
        number={'suffix': '%'},
        domain={'x': [0, 1], 'y': [0, 1]},
        title={'text': "Risk Meter (Probability of default)"},
        gauge={
            'axis': {'range': [0, 100]},
            'bar': {'color': "rgba(255, 255, 0, 0.6)"},
            'steps': [
                {'range': [0, 32], 'color': "rgba(0, 128, 0, 0.6)"},
                {'range': [32, 100], 'color': "rgba(255, 0, 0, 0.6)"},
            ],
            'threshold': {
                'line': {'color': "black", 'width': 4},
                'thickness': 0.75,
                'value': st.session_state.probability
            }
        }
    ))
    
    # Render the chart in Streamlit
    st.plotly_chart(fig)
    explainer = shap.Explainer(model)
    shap_values = explainer(st.session_state.X)

    # Get the mean absolute SHAP values for each feature
    feature_importance = pd.DataFrame({
        'Feature': st.session_state.X.columns,
        'Mean Impact': np.abs(shap_values.values).mean(axis=0),  # Use absolute values for ranking
        'Impact': shap_values.values[0],
    })

    # Sort features based on their mean absolute SHAP impact (top 3)
    feature_importance = feature_importance.sort_values(by='Mean Impact', ascending=False).head(3)
    feature_sum = np.sum(np.abs(shap_values.values[0]))
    st.subheader("Top 3 Most Impactful Features")

    cols = st.columns(len(feature_importance)+100)
    # Display each feature with thumbs up/thumbs down based on its impact
    for i, row in feature_importance.iterrows():
        feature = row['Feature']
        mean_impact = row['Mean Impact']
        impact = row['Impact']
        percentage_value = round((mean_impact / feature_sum) * 100)
        impact_icon = "👍" if impact < 0 else "👎"
        # Color code: green for positive, red for negative
        color = "green" if impact < 0 else "red"
        
        # Create a row for each feature
        st.markdown(
            f"""
            <div style="text-align: center; margin-bottom: 20px;">
                <strong>{feature}</strong><br>
                <span style="color: {color}; font-size: 20px;">
                    {impact_icon} {percentage_value}%
                </span>
            </div>
            """,
            unsafe_allow_html=True
        )

    st.subheader("Feature analysis")
    col1, col2 = st.columns(2)
    #Feature analysis
    state_name = state_abbreviation_to_name.get(state, "Unknown State")
    state_default_rate = stateData.loc[stateData["StateName"] == state_name.lower(), "default_rate"].values[0]
    industry_default_rate = industryData.loc[industryData["Industry"] == industry, "default_rate"].values[0]
    with col1: 
        st.markdown(
            f"""
            <div style="text-align: center;">
                <h3>Selected State: {state_name}</h3><br>
                <p>Businesses in the state of {state_name} have a default rate of {state_default_rate*100:.2f}% compared to the average 26.69% default rate</p>
            </div>
            """,
            unsafe_allow_html=True
        )
        if urbanRural == 1:
            st.markdown(
                f"""
                <div style="text-align: center;">
                    <h3>Urban Businesses</h3><br>
                    <p>Businesses that are urban have a higher default rate than rural businses by ~6%</p>
                </div>
                """,
                unsafe_allow_html=True
            )
        else:
            st.markdown(
                f"""
                <div style="text-align: center;">
                    <h3>Rural Businesses</h3><br>
                    <p>Businesses that are rural have a lower default rate than urban businses by ~6%</p>
                </div>
                """,
                unsafe_allow_html=True
            )
        if realEstate == 1:
            st.markdown(
                f"""
                <div style="text-align: center;">
                    <h3>Loans backed by real estate</h3><br>
                    <p>Loans backed by real estate have a lower default rate than loans not backed by real estate by ~27%</p>
                </div>
                """,
                unsafe_allow_html=True
            )
        else:
            st.markdown(
                f"""
                <div style="text-align: center;">
                    <h3>Loans not backed by real estate</h3><br>
                    <p>Loans not backed by real estate have a higher default rate than loans backed by real estate by ~27%</p>
                </div>
                """,
                unsafe_allow_html=True
            )
    with col2:
        st.markdown(
            f"""
            <div style="text-align: center;">
                <h3>Selected Industry: {industry}</h3><br>
                <p>Businesses in the {industry} industry have a default rate of {industry_default_rate*100:.2f}% compared to the average 26.69% default rate</p>
            </div>
            """,
            unsafe_allow_html=True
        )
        if franchiseCode == 1:
            st.markdown(
                f"""
                <div style="text-align: center;">
                    <h3>Franchised Businesses</h3><br>
                    <p>Businesses that are franchised have a lower default rate than Non-Franchised Businesses by ~8%</p>
                </div>
                """,
                unsafe_allow_html=True
            )
        else:
            st.markdown(
                f"""
                <div style="text-align: center;">
                    <h3>Non-Franchised Businesses</h3><br>
                    <p>Businesses that are Non-Franchised have a higher default rate than Franchised Businesses businses by ~8%</p>
                </div>
                """,
                unsafe_allow_html=True
            )
        if lowDoc == 1:
            st.markdown(
                f"""
                <div style="text-align: center;">
                    <h3>Loans in the LowDoc program</h3><br>
                    <p>Loans in the LowDoc program have a lower default rate than those not in it by ~11%</p>
                </div>
                """,
                unsafe_allow_html=True
            )
        else:
            st.markdown(
                f"""
                <div style="text-align: center;">
                    <h3>Loans not in the LowDoc program</h3><br>
                    <p>Loans not in the LowDoc program have a lower default rate than those in it by ~11%</p>
                </div>
                """,
                unsafe_allow_html=True
            )